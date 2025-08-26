#!/bin/sh

if [ "$APPLY_INTEREST" = "true" ]; then
  echo "Starting interest calculation process..."
  ./main --apply-interest &
fi

echo "Starting web server..."
exec python3 -m uvicorn app:app --host 0.0.0.0 --port 8000