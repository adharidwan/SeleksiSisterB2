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
COPY start.sh ./

RUN chmod +x start.sh

ARG APPLY_INTEREST_ARG=false
ENV APPLY_INTEREST=$APPLY_INTEREST_ARG

RUN python3 -m pip install -r requirements.txt

RUN cobc -free -x -o main main.cob

EXPOSE 8000

CMD ["./start.sh"]