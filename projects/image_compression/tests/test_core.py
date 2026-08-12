import numpy as np

from kmeans_image.core import simplify_image

# Check if the number of colors is equal or smaller than k
def test_color_reduction():
    img = np.array(
        [
            [[255, 0, 0], [0, 255, 0], [0, 0, 255]],
            [[255, 255, 0], [255, 0, 255], [0, 255, 255]],
            [[128, 0, 0], [0, 128, 0], [0, 0, 128]],
        ],
        dtype=np.uint8,
    )

    k = 3

    result = simplify_image(
        img,
        k=k,
        max_iter=20,
        threshold=0,
        seed=0,
    )

    colors = np.unique(
        result.reshape(-1, 3),
        axis=0,
    )

    assert len(colors) <= k
