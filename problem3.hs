import Data.Foldable
import Data.List
import qualified Data.Set as Set

data Direction = Up | Down | Rght | Lft

data PathEl = U Int | D Int | R Int | L Int

wireA :: [PathEl]
wireA = [R 8, U 5, L 5, D 3]

wireB :: [PathEl]
wireB = [U 7, R 6, D 4, L 4]

wire1 :: [PathEl]
wire1 = [R 997, D 99, R 514, D 639, L 438, D 381, L 251, U 78, L 442, D 860, R 271, U 440, L 428, U 482, R 526, U 495, R 361, D 103, R 610, D 64, L 978, U 587, L 426, D 614, R 497, D 116, R 252, U 235, R 275, D 882, L 480, D 859, L 598, D 751, R 588, D 281, R 118, U 173, L 619, D 747, R 994, U 720, L 182, U 952, L 49, D 969, R 34, D 190, L 974, U 153, L 821, U 593, L 571, U 111, L 134, U 111, R 128, D 924, R 189, U 811, R 100, D 482, L 708, D 717, L 844, U 695, R 277, D 81, L 107, U 831, L 77, U 609, L 629, D 953, R 491, D 17, R 160, U 468, R 519, D 41, R 625, D 501, R 106, D 500, R 473, D 244, R 471, U 252, R 440, U 326, R 710, D 645, L 190, D 670, L 624, D 37, L 46, D 242, L 513, D 179, R 192, D 100, R 637, U 622, R 322, U 548, L 192, D 85, L 319, D 717, L 254, D 742, L 756, D 624, L 291, D 663, R 994, U 875, R 237, U 304, R 40, D 399, R 407, D 124, R 157, D 415, L 405, U 560, R 607, U 391, R 409, U 233, R 305, U 346, L 233, U 661, R 213, D 56, L 558, U 386, R 830, D 23, L 75, D 947, L 511, D 41, R 927, U 856, L 229, D 20, L 717, D 830, R 584, U 485, R 536, U 531, R 946, U 942, R 207, D 237, L 762, U 333, L 979, U 29, R 635, D 386, R 267, D 260, R 484, U 887, R 568, D 451, R 149, U 92, L 379, D 170, R 135, U 799, L 617, D 380, L 872, U 868, R 48, U 279, R 817, U 572, L 728, D 792, R 833, U 788, L 940, D 306, R 230, D 570, L 137, U 419, L 429, D 525, L 730, U 333, L 76, D 435, R 885, U 811, L 937, D 320, R 152, U 906, L 461, U 227, L 118, U 951, R 912, D 765, L 638, U 856, L 193, D 615, L 347, U 303, R 317, U 23, L 139, U 6, L 525, U 308, L 624, U 998, R 753, D 901, R 556, U 428, L 224, U 953, R 804, D 632, L 764, U 808, L 487, U 110, L 593, D 747, L 659, D 966, R 988, U 217, L 657, U 615, L 425, D 626, L 194, D 802, L 440, U 209, L 28, U 110, L 564, D 47, R 698, D 938, R 13, U 39, R 703, D 866, L 422, D 855, R 535, D 964, L 813, D 405, R 116, U 762, R 974, U 568, R 934, U 574, R 462, D 968, R 331, U 298, R 994, U 895, L 204, D 329, R 982, D 83, L 301, D 197, L 36, U 329, R 144, U 497, R 300, D 551, L 74, U 737, R 591, U 374, R 815, U 771, L 681]

wire2 :: [PathEl]
wire2 = [L 997, D 154, R 652, U 379, L 739, U 698, R 596, D 862, L 125, D 181, R 786, U 114, R 536, U 936, L 144, U 936, R 52, U 899, R 88, D 263, R 122, D 987, L 488, U 303, R 142, D 556, L 691, D 769, L 717, D 445, R 802, U 294, L 468, D 13, R 301, D 651, L 242, D 767, R 465, D 360, L 144, D 236, R 59, U 815, R 598, U 375, R 645, U 905, L 714, U 440, R 932, D 160, L 420, U 361, L 433, D 485, L 276, U 458, R 760, D 895, R 999, U 263, R 530, U 691, L 918, D 790, L 150, U 574, R 800, U 163, R 478, U 112, L 353, U 30, L 763, U 239, L 353, U 619, R 669, D 822, R 688, U 484, L 678, D 88, R 946, D 371, L 209, D 175, R 771, D 85, R 430, U 16, R 610, D 326, R 836, U 638, L 387, D 996, L 758, U 237, L 476, U 572, L 456, U 579, L 457, D 277, L 825, U 204, R 277, U 267, L 477, D 573, L 659, D 163, L 516, D 783, R 762, U 146, L 387, U 700, R 911, U 335, L 115, D 887, R 677, U 312, R 707, U 463, L 743, U 358, L 715, D 603, R 966, U 21, L 857, D 680, R 182, D 977, L 279, U 196, R 355, D 624, L 434, U 410, R 385, U 47, L 999, D 542, L 453, D 735, R 781, U 115, R 814, U 110, R 344, D 139, R 899, D 650, L 118, D 774, L 227, D 140, L 198, D 478, R 115, D 863, R 776, D 935, R 473, U 722, R 555, U 528, L 912, U 268, R 776, D 223, L 302, D 878, R 90, U 52, L 595, U 898, L 210, U 886, R 161, D 794, L 846, U 404, R 323, U 616, R 559, U 510, R 116, D 740, L 554, U 231, R 54, D 328, L 56, U 750, R 347, U 376, L 148, U 454, L 577, U 61, L 772, D 862, R 293, U 82, L 676, D 508, L 53, D 860, L 974, U 733, R 266, D 323, L 75, U 218, L 390, U 757, L 32, D 455, R 34, D 363, L 336, D 67, R 222, D 977, L 809, D 909, L 501, U 483, L 541, U 923, R 97, D 437, L 296, D 941, L 652, D 144, L 183, U 369, L 629, U 535, L 825, D 26, R 916, U 131, R 753, U 383, L 653, U 631, R 280, U 500, L 516, U 959, R 858, D 830, R 357, D 87, R 885, D 389, L 838, U 550, R 262, D 529, R 34, U 20, L 25, D 553, L 884, U 806, L 800, D 988, R 499, D 360, R 435, U 381, R 920, D 691, R 373, U 714, L 797, D 677, L 490, D 976, L 734, D 585, L 384, D 352, R 54, D 23, R 339, D 439, L 939, U 104, L 651, D 927, L 152]

main :: IO ()
main = do
  let points1 = Set.fromList $ trackWire wire1
  let points2 = Set.fromList $ trackWire wire2
  let intersections = Set.intersection points1 points2
  putStrLn $ show intersections
  let sorted = sortOn manhattan $ Set.toList intersections
  putStrLn $ show $ sorted
  putStrLn $ show $ manhattan $ sorted !! 1

manhattan :: (Int, Int) -> Int
manhattan (x,y) = abs x + abs y

trackWire :: [PathEl] -> [(Int, Int)]
trackWire path = foldl' (flip extendWire) [(0,0)] path

extendWire :: PathEl -> [(Int, Int)] -> [(Int, Int)]
extendWire pathEl points = case pathEl of
  U s -> iterate (go Up) points !! s
  D s -> iterate (go Down) points !! s
  L s -> iterate (go Lft) points !! s
  R s -> iterate (go Rght) points !! s

go :: Direction -> [(Int, Int)] -> [(Int, Int)]
go dir points@((x,y) : _) = case dir of
  Up -> (x, y+1) : points
  Down -> (x, y-1) : points
  Lft -> (x-1, y) : points
  Rght -> (x+1, y) : points
