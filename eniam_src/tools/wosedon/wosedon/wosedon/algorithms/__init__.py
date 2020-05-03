# Implementation of Static pagerank from GraphTool
from wosedon.algorithms.gt_static_pr import GTStaticPR
# Personalized Page Rank imeplementation into graphTool
from wosedon.algorithms.gt_personalized_pr import GTPersonalizedPR
# Personalized Page Rank Word-to-word imeplementation into graphTool
from wosedon.algorithms.gt_personalized_w2w_pr import GTPersonalizedW2WPR
# Normalized PPR into graph tools - only initial vector v is normalized
from wosedon.algorithms.gt_personalized_pr_norm2 import GTPersonalizedPRNorm2

# It looks, that this approach is ctreated by ours
from wosedon.algorithms.gt_personalized_pr_norm import GTPersonalizedPRNorm

# I don't known who is the author of this idea
# Normalized PPR into graph tools - into each loop
from wosedon.algorithms.gt_personalized_pr_norm_it import GTPersonalizedPRNormIt

# Implementation of our ideas for PPR modification
# Normalized PPR with persv modificated into each loop (v = v*Pr)
from wosedon.algorithms.gt_pers_pr_norm_mod_v import GTPersPRNormModV
# Normalized PPR with persv modificated into each loop (v = v*Pr) but
# in each iteration the personalized vector is normalized to range <0,1>
from wosedon.algorithms.gt_pers_pr_norm_it_mod_v import GTPersPRNormItModV
# Normalized PPR with persv modificated into each loop (v = v*Pr) but
# in each iteration the personalized vector is normalized to range <0,1>
# In each loop the ranking is normalized to <0,1> range
from wosedon.algorithms.gt_pers_pr_norm_it_mod_v_rank_norm import GTPersPRNormItModVRankNorm

# usuwanie najgorszego wezla w rankingu - usuwanie az do 50% wartosci rankingu
from wosedon.algorithms.gt_personalized_pr_norm_reduction import GTPersonalizedPRNormReduction

# algorytm Lesk
from wosedon.algorithms.lesk_alg import LeskAlg

from wosedon.algorithms.gt_personalized_w2w_pr_norm import GTPersonalizedW2WPRNorm

from wosedon.algorithms.gt_personalized_pr_norm_two_step import GTPersonalizedPRNormTwoStep

# SUDOKU
from wosedon.algorithms.gt_sudoku_run2 import GTSUDOKURun2

# paintball dla WSD
from wosedon.algorithms.paintball_wsd import PaintballWSD
