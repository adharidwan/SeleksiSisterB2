FROM ubuntu:22.04

RUN apt-get update && apt-get install -y --no-install-recommends \
    gcc \
    gnucobol \
    python3 \
    python3-pip \
    python3-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /workspace

COPY app.py index.html main.cob accounts.txt requirements.txt ./

# Install Python dependencies
RUN python3 -m pip install -r requirements.txt

# Compile COBOL program
RUN cobc -free -x -o main main.cob

EXPOSE 8000

CMD ["python3", "-m", "uvicorn", "app:app", "--host", "0.0.0.0", "--port", "8000"]