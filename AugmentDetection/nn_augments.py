import torch
import torch.nn as nn
import torch.optim as optim
from torchvision import models, transforms
from torch.utils.data import Dataset, DataLoader
from PIL import Image

# Define your custom dataset class
class ScreenDataset(Dataset):
    def __init__(self, data, transform=None):
        self.data = data
        self.transform = transform

    def __len__(self):
        return len(self.data)

    def __getitem__(self, idx):
        img_path, labels = self.data[idx]
        image = Image.open(img_path).convert('RGB')
        if self.transform:
            image = self.transform(image)
        return image, labels

# Define your CNN model architecture
class MyModel(nn.Module):
    def __init__(self):
        super(MyModel, self).__init__()
        # Define your layers here
        self.features = models.resnet18(pretrained=True)  # Use pretrained weights
        self.fc = nn.Linear(1000, 1)  # Output layer with 9 units

    def forward(self, x):
        x = self.features(x)
        x = self.fc(x)
        return x

# Define transformations for data preprocessing and augmentation
train_transform = transforms.Compose([
    transforms.Resize((224, 224)),  # Resize images to fit the input size of the CNN
    transforms.RandomHorizontalFlip(),  # Randomly flip images horizontally
    transforms.RandomRotation(10),  # Randomly rotate images by up to 10 degrees
    transforms.ColorJitter(brightness=0.2, contrast=0.2, saturation=0.2, hue=0.1),  # Randomly adjust brightness, contrast, saturation, and hue
    transforms.ToTensor(),  # Convert images to tensors
    transforms.Normalize(mean=[0.485, 0.456, 0.406], std=[0.229, 0.224, 0.225])  # Normalize images
])

# Create your dataset and dataloader
train_data = [('images/old1.jpg', [1])]  # List of tuples containing (image_path, label)
train_dataset = ScreenDataset(train_data, transform=train_transform)
train_loader = DataLoader(train_dataset, batch_size=32, shuffle=True)

# Instantiate your model
model = MyModel()

# Define loss function and optimizer
criterion = nn.CrossEntropyLoss()
optimizer = optim.Adam(model.parameters(), lr=0.001)

# Train your model
num_epochs = 2
for epoch in range(num_epochs):
    for images, labels in train_loader:
        optimizer.zero_grad()
        outputs = model(images)
        labels = torch.tensor([item for sublist in labels for item in sublist])  # Flatten labels list
        loss = criterion(outputs, labels)
        loss.backward()
        optimizer.step()

# Save your trained model
torch.save(model.state_dict(), 'my_model_with_augmentation.pth')

# Once trained, you can use this model for inference on new screenshots
