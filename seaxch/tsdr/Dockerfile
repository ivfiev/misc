FROM rocm/pytorch:latest

WORKDIR /tsdr

RUN pip install --no-cache-dir sentence-transformers


ENV PYTHONUNBUFFERED=1

CMD ["python", "main.py"]
