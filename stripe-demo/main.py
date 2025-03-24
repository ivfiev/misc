import stripe
import uvicorn
from fastapi import FastAPI, HTTPException, Request
from dotenv import load_dotenv

load_dotenv()

STRIPE_WEBHOOK_SECRET = ''
app = FastAPI()

@app.post("/webhook")
async def stripe_webhook(request: Request):
    payload = await request.body()
    sig_header = request.headers.get("stripe-signature")
    try:
        event = stripe.Webhook.construct_event(
            payload, sig_header, STRIPE_WEBHOOK_SECRET
        )
    except (stripe.error.SignatureVerificationError, ValueError):
        raise HTTPException(status_code=400, detail="Invalid webhook signature")
    print(event["type"])
    # if event["type"] == "payment_intent.succeeded":
    #     print("Payment succeeded!", event["data"]["object"])
    
    return {"status": "success"}

if __name__ == "__main__":
    uvicorn.run(app, host="localhost", port=8000)
