import argparse
from pathlib import Path
from skimage import io
from kmeans_image.core import simplify_image


def parse_args() -> argparse.Namespace:
    """Parse command-line arguments."""
    parser = argparse.ArgumentParser(
        description="Simplify an image's color palette using K-means clustering."
    )

    parser.add_argument(
        "image_path",
        type=Path,
        help="Path to the input RGB image.",
    )

    parser.add_argument(
        "output_path",
        type=Path,
        help="Path where the simplified image will be saved.",
    )

    parser.add_argument(
        "--k",
        type=int,
        default=12,
        help="Number of colors/clusters. Default: 12.",
    )

    parser.add_argument(
        "--max-iter",
        type=int,
        default=15,
        help="Maximum number of K-means iterations. Default: 15.",
    )

    parser.add_argument(
        "--threshold",
        type=float,
        default=25.0,
        help="Squared centroid-movement convergence threshold. Default: 25.",
    )

    parser.add_argument(
        "--seed",
        type=int,
        default=0,
        help="Random seed. Default: 0.",
    )

    return parser.parse_args()


def run() -> None:
    """Run the command-line interface."""
    args = parse_args()

    img = io.imread(args.image_path)

    new_img = simplify_image(
        img,
        args.k,
        args.max_iter,
        args.threshold,
        args.seed,
    )

    io.imsave(
        args.output_path,
        new_img,
        check_contrast=False,
    )

    print(f"Saved simplified image to: {args.output_path}")


if __name__ == "__main__":
    run()
