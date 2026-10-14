# SPDX-License-Identifier: AGPL-3.0-only
import numpy as np

def resize_nearest(array, size_wh):
    """Pixel-center nearest neighbor with dtype-independent integer indexing."""
    width, height = map(int, size_wh)
    src_h, src_w = array.shape
    yi = np.minimum(((2*np.arange(height,dtype=np.int64)+1)*src_h)//(2*height),src_h-1)
    xi = np.minimum(((2*np.arange(width,dtype=np.int64)+1)*src_w)//(2*width),src_w-1)
    return array[np.ix_(yi,xi)]
