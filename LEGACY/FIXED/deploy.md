# Manual Deployment Guide: Rai Stone Banking on AKS

This guide provides a complete, step-by-step process for manually deploying the Rai Stone Banking application to Azure Kubernetes Service (AKS). This method is ideal for understanding each component of the deployment process.

## Prerequisites

Before you begin, ensure you have the following tools installed and configured on your local machine:

### Azure CLI
- **Requirement**: Authenticated to your Azure account
- **Install Guide**: [Install the Azure CLI](https://docs.microsoft.com/en-us/cli/azure/install-azure-cli)
- **Login Command**: `az login`

### Docker Desktop
- **Requirement**: Must be installed and running
- **Install Guide**: [Get Docker](https://docs.docker.com/get-docker/)

### kubectl
- **Requirement**: The Kubernetes command-line tool
- **Install Command** (via Azure CLI): `az aks install-cli`

## Step 1: Create Core Azure Resources

First, we'll set up the foundational resources: a resource group to hold everything and a container registry for our Docker image.

### Define Variables
*(Optional, but makes the commands easier to copy/paste)*

```bash
#Example
RESOURCE_GROUP="rg-rai-stone-banking"
LOCATION="southeastasia"
ACR_NAME="acrraistonebanking$(date +%s | tail -c 5)" # Creates a unique name
AKS_NAME="aks-rai-stone-banking"
```

### Create the Resource Group
```bash
az group create --name $RESOURCE_GROUP --location "$LOCATION"
```

### Create the Azure Container Registry (ACR)
```bash
az acr create --resource-group $RESOURCE_GROUP --name $ACR_NAME --sku Basic
```

## Step 2: Build and Push the Docker Image

Because student subscriptions often block the `az acr build` command, we will build the image on our local machine and push it to the registry.

### Log Docker into your ACR
This command authenticates your local Docker client with your new Azure Container Registry.

```bash
az acr login --name $ACR_NAME
```

### Build the Docker Image Locally
Navigate to your application's root directory (where the Dockerfile is located) and run the build command. We tag it with the full path to the registry.

```bash
docker build -t ${ACR_NAME}.azurecr.io/rai-stone-banking:latest .
```

### Push the Image to ACR
Upload the locally-built image to your registry in Azure.

```bash
docker push ${ACR_NAME}.azurecr.io/rai-stone-banking:latest
```

## Step 3: Create the Kubernetes Cluster (AKS)

Now we create the Kubernetes cluster and configure kubectl to connect to it.

### Create the AKS Cluster
This command provisions the cluster and, importantly, grants it the necessary permissions (`--attach-acr`) to pull images from the ACR we created earlier. This step can take 5-10 minutes.

```bash
az aks create \
  --resource-group $RESOURCE_GROUP \
  --name $AKS_NAME \
  --node-count 1 \
  --attach-acr $ACR_NAME \
  --generate-ssh-keys
```

### Get Cluster Credentials for kubectl
This command downloads the cluster's connection information and configures your local kubectl to point to your new AKS cluster.

```bash
az aks get-credentials --resource-group $RESOURCE_GROUP --name $AKS_NAME
```

## Step 4: Deploy the Application to Kubernetes

With the infrastructure ready and the image in our registry, we can now deploy the application.

### Update the Kubernetes Manifest File
Your `k8s-deploy.yaml` file needs to know the full path to your image in ACR. This sed command updates it for you.

```bash
sed -i.bak "s|rai-stone-banking:latest|${ACR_NAME}.azurecr.io/rai-stone-banking:latest|g" k8s-deploy.yaml
```

> **Note**: If you prefer, you can manually edit the `k8s-deploy.yaml` file and change the `image:` line yourself.

### Apply the Manifest to the Cluster
This command tells Kubernetes to create the resources (Deployment, Service, etc.) defined in your YAML file.

```bash
kubectl apply -f k8s-deploy.yaml
```

## Step 5: Verify the Deployment

Check that your application is running and find its public IP address.

### Check Pod Status
You should see the status change from `ContainerCreating` to `Running`.

```bash
kubectl get pods -n rai-stone-banking -w
```

*(Press Ctrl+C to exit the watch (`-w`) mode.)*

### Get the Service's External IP
The `EXTERNAL-IP` will show `<pending>` for a few minutes while Azure provisions a public IP for the load balancer.

```bash
kubectl get service rai-stone-banking-service -n rai-stone-banking
```

Once the IP address appears, you can access your application by browsing to that IP.

## Step 6: Cleanup (Important!)

To avoid ongoing costs, delete all the resources when you are finished.

### Delete the Entire Resource Group
This single command will permanently delete the AKS cluster, ACR, and all associated resources.

```bash
az group delete --name $RESOURCE_GROUP --yes
```

---

## Troubleshooting Tips

- If pods are stuck in `ContainerCreating` status, check the events with: `kubectl describe pod <pod-name> -n rai-stone-banking`
- If the external IP remains `<pending>` for more than 10 minutes, check the service events: `kubectl describe service rai-stone-banking-service -n rai-stone-banking`
- Verify your Docker image was pushed successfully: `az acr repository list --name $ACR_NAME`

