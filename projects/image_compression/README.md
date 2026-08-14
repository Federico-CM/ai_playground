![K-Means Image Segmentation Diagram](https://raw.githubusercontent.com/Federico-CM/ml_experiments/main/projects/image_compression/k_means_info.png)


## What is this?
This project contains a python package that implements K-means clustering and performs image segmentation.
The goal is simple: Given an image, can we group pixels into K groups based on how similar they are by color, and produce a segmented version of the image?

## Why is this interesting?
This project shows how a computer can learn to sort things into categories based on measurable traits.
In everyday terms, this is the same kind of task used in:

- Image compresion
- Customer segmentation
- Market basket analysis
- Medical diagnosis support
- Natural language processing

## What does this project show?
This project is an example of:
- Principles of Software engineering (package formating, automated testing)
- How a commonly used algorithm works under the hood
- How computers can group similar data points into clusters
- How an image can be treated like a dataset (each pixel is a data row)
- How unsupervised learning works (no labeled “correct answer” needed)

# Technical Details
## How do I install the package?
The package requires Python 3.10 or newer.
Creating a virtual environment is recommended:

python3 -m venv .venv 
source .venv/bin/activate

Install the package in editable mode:
python -m pip install -e .

For development, including the test dependencies, use:
python -m pip install -e ".[dev]"

## Usage
After installation, the image simplifier is available through the kmeans-image command:
kmeans-image INPUT_IMAGE OUTPUT_IMAGE

For example:
kmeans-image examples/wakayama.jpg output.png

The K-means parameters can also be configured from the command line:
kmeans-image examples/wakayama.jpg output.png \ 
    --k 8 \
    --max-iter 20 \
    --threshold 10 \
    --seed 42

Use the built-in help for a complete description of the available options:
kmeans-image --help

## Testing
The test suite uses pytest.
You can check if the code (and changes implemented) work by running:
pytest

## How do I interpret the results?
The script produces two images: the original image and a segmented version where the number of colors has been reduced. The segmentation works by grouping similar colors together and replacing them with a representative color, which simplifies the overall image. Instead of using thousands or millions of colors, the image is recreated using only K main colors. When K is small, the image looks more simplified and “poster-like,” and some fine details may be lost. When K is larger, the segmented image looks much closer to the original because more color variation is preserved. 
