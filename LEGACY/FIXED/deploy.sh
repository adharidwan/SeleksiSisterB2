#!/bin/bash
set -e

# Variables
RESOURCE_GROUP="rg-rai-stone-banking"
LOCATION="Southeast Asia"
ACR_NAME="acrraistonebanking$(date +%s | tail -c 4)"
AKS_NAME="aks-rai-stone-banking"

echo "🗿 Starting Rai Stone Banking deployment..."

# Create resources
az group create --name $RESOURCE_GROUP --location "$LOCATION"
az acr create --resource-group $RESOURCE_GROUP --name $ACR_NAME --sku Basic --admin-enabled true
az aks create --resource-group $RESOURCE_GROUP --name $AKS_NAME --node-count 2 --attach-acr $ACR_NAME --generate-ssh-keys
az aks get-credentials --resource-group $RESOURCE_GROUP --name $AKS_NAME

# Build and deploy
ACR_LOGIN_SERVER=$(az acr show --name $ACR_NAME --query loginServer --output tsv)
az acr build --registry $ACR_NAME --image rai-stone-banking:latest .
sed -i "s|rai-stone-banking:latest|$ACR_LOGIN_SERVER/rai-stone-banking:latest|g" k8s-deploy.yaml
kubectl apply -f k8s-deploy.yaml

echo "✅ Deployment complete! Getting external IP..."
kubectl get service rai-stone-banking-service -n rai-stone-banking -w