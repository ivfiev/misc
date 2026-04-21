import torch
from transformers import AutoModelForCausalLM, AutoTokenizer, pipeline

model = AutoModelForCausalLM.from_pretrained(
    "microsoft/Phi-3-mini-4k-instruct",
    # device_map="cpu",
    torch_dtype="auto",
    trust_remote_code=False,
)
tokenizer = AutoTokenizer.from_pretrained("microsoft/Phi-3-mini-4k-instruct")

generator = pipeline(
    "text-generation",
    model=model,
    tokenizer=tokenizer,
    dtype=torch.bfloat16,
    device_map="auto",
)

prompt = "Capital of Paris is"

toks = tokenizer(prompt, return_tensors="pt").input_ids.to("cuda")
out = model.model(toks)
lm_head_out = model.lm_head(out.last_hidden_state)
tok = lm_head_out[0, -1, :].argmax(-1)
print(tok)
print(tokenizer.decode(tok))
