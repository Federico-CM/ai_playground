"""
K-means image simplifier.

This script loads an RGB image and reduces its color palette using
K-means clustering. The result is a simplified image with K colors.
"""
from pathlib import Path
import numpy as np
from skimage import io

# ======================
# I/O + Validation
# ======================
def validate_data(
    img: np.ndarray,
    k: int,
    max_iter: int,
    threshold: float
) -> None:
    """
    Validate image and hyperparameters.

    Raises ValueError if any condition is invalid.
    """

    if img.ndim != 3 or img.shape[2] != 3:
        raise ValueError(
            "This script supports only RGB images, no transparency."
        )

    h, w = img.shape[:2]
    n_pixels = h * w

    if not (type(k) is int) or k <= 1:
        raise ValueError("K must be a non 1, positive integer.")

    if k > n_pixels:
        raise ValueError(
            f"K ({k}) cannot exceed number of pixels ({n_pixels})."
        )

    if not (type(max_iter) is int) or max_iter <= 0:
        raise ValueError("MAX_ITER must be a positive integer.")

    if not (type(threshold) in (int,float)) or threshold < 0:
        raise ValueError("THRESHOLD must be a float >= 0.")

# ======================
# Image handling
# ======================
def save_image(img: np.ndarray, output_path: Path) -> None:
    """Save an image to disk."""
    io.imsave(output_path, img, check_contrast=False)
    print(f"Saved simplified image to: {output_path}")


# ======================
# K-means helpers
# ======================
def init_centroids(img_f: np.ndarray, k: int):
    """Select k random pixels from the image as initial centroids."""
    y, x = img_f.shape[:2]
    ys = np.random.randint(0, y, size=k)
    xs = np.random.randint(0, x, size=k)
    centroids = img_f[ys, xs]
    return centroids


def compute_distances(img_f: np.ndarray, centroids: np.ndarray) -> np.ndarray:
    """Compute squared Euclidean distances from each pixel to each centroid."""
    diff = img_f[:, :, None, :] - centroids[None, None, :, :]
    distances = (diff * diff).sum(axis=3, dtype=np.float32)
    return distances


def assign_labels(distances: np.ndarray) -> np.ndarray:
    """Assign each pixel to the nearest centroid. Returns (H, W) int array."""
    labels = np.argmin(distances, axis=2)
    return labels


def recompute_centroids(img_f, labels, k):
    """Recalculate centroids for k clusters."""
    new_centroids = np.zeros((k, 3), dtype=np.float32)

    for i in range(k):
        mask = labels == i

        if not np.any(mask):
            new_centroids[i] = init_centroids(img_f, 1)[0]
        else:
            new_centroids[i] = img_f[mask].mean(axis=0)

    return new_centroids


def check_convergence(new_centroids, centroids, threshold, iteration):
    """Checks whether centroid movement is below a threshold."""
    max_movement = ((new_centroids - centroids) ** 2).sum(axis=1).max()

    if max_movement < threshold:
        print("Reached convergence at iteration:", iteration + 1)
        return True

    print("Finished iteration:", iteration + 1)
    return False


def apply_centroids(img, labels, centroids, k):
    """Replace pixels in img with their corresponding centroid values."""
    centroids = centroids.round().clip(0, 255).astype(img.dtype)

    new_img = img.copy()

    for i in range(k):
        new_img[labels == i] = centroids[i]

    return new_img

# ======================
# The main function
# ======================
def simplify_image(
    img: np.ndarray,
    k: int,
    max_iter: int,
    threshold: float,
    seed: int,
) -> np.ndarray:
    """Reduce an RGB image to k colors using K-means clustering."""
    validate_data(img, k, max_iter, threshold)

    img_f = img.astype(np.float32)

    np.random.seed(seed)
    centroids = init_centroids(img_f, k)

    for iteration in range(max_iter):
        distances = compute_distances(img_f, centroids)
        labels = assign_labels(distances)

        new_centroids = recompute_centroids(img_f, labels, k)

        converged = check_convergence(
            new_centroids,
            centroids,
            threshold,
            iteration,
        )

        centroids = new_centroids

        if converged:
            break

    return apply_centroids(img, labels, centroids, k)
