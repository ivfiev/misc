from PIL import Image
import numpy as np
import torch
from transformers import CLIPTextModel, CLIPTokenizer
from diffusers import UNet2DConditionModel, AutoencoderKL
from diffusers import EulerAncestralDiscreteScheduler

device = "cuda"
model_id = "runwayml/stable-diffusion-v1-5"
tokenizer = CLIPTokenizer.from_pretrained(model_id, subfolder="tokenizer")
text_encoder = CLIPTextModel.from_pretrained(model_id, subfolder="text_encoder", torch_dtype=torch.float16).to(device)
unet = UNet2DConditionModel.from_pretrained(model_id, subfolder="unet", torch_dtype=torch.float16).to(device)
vae = AutoencoderKL.from_pretrained(model_id, subfolder="vae", torch_dtype=torch.float16).to(device)
scheduler = EulerAncestralDiscreteScheduler.from_pretrained(model_id, subfolder="scheduler")

prompt = "dog taking a shit"
text_inputs = tokenizer(prompt, padding="max_length", max_length=tokenizer.model_max_length, return_tensors="pt")

with torch.inference_mode():
    text_embeddings = text_encoder(text_inputs.input_ids.to(device))[0]

guidance_scale = 7

uncond_input = tokenizer("", padding="max_length", max_length=tokenizer.model_max_length, return_tensors="pt")

with torch.inference_mode():
    uncond_embeddings = text_encoder(uncond_input.input_ids.to(device))[0]

# create uncond & cond embeds to guide diffusion
embeddings = torch.cat([uncond_embeddings, text_embeddings])

batch_size = 1
height = 512
width = 512

# 1 latent 4x64x64
latents = torch.randn(
    (batch_size, unet.config.in_channels, height // 8, width // 8),
    device=device,
    dtype=torch.float16,
)

num_steps = 20
scheduler.set_timesteps(num_steps, device=device)

latents = latents * scheduler.init_noise_sigma

for t in scheduler.timesteps:
    # double latents, uncold & cond - for unet
    latent_input = torch.cat([latents] * 2)
    latent_input = scheduler.scale_model_input(latent_input, t)
    with torch.inference_mode():
        # 2 noise preds from unet
        noise_pred = unet(latent_input, t, encoder_hidden_states=embeddings).sample
    noise_uncond, noise_text = noise_pred.chunk(2)
    # next step vector - default denoise + bias towards proompt
    # why not 'noise_text + ...'? schedulers prefer an uncond component for some reason
    noise_pred = noise_uncond + guidance_scale * (noise_text - noise_uncond)
    latents = scheduler.step(noise_pred, t, latents).prev_sample

# sth to do with normalizing variance
latents = latents / 0.18215

with torch.inference_mode():
    image = vae.decode(latents).sample

image = (image / 2 + 0.5).clamp(0, 1)
image = image.cpu().permute(0, 2, 3, 1).numpy()[0]
image_uint8 = (image * 255).astype(np.uint8)
img = Image.fromarray(image_uint8)
img.save("output.png")
