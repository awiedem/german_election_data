#!/usr/bin/env python3
"""
NRW 1947 Landtagswahl — Compile WK-level data, aggregate to Kreis level.
Election date: 1947-04-20
150 Wahlkreise across 6 Regierungsbezirke.
Party columns: CDU, SPD, FDP, KPD, Z(entrum), DRP, RWVP, Unab(hängige)

Source: Nordrhein-Westfalen_1947_Landtagswahl.pdf (a) rows)
Visual reading of 600 DPI images — OCR failed on this scan.
"""
import csv, sys, os

# --- WK party data (a) rows only) ---
# Format: {wk_num: {'CDU':..., 'SPD':..., ...}}

wk = {}

# WK 1-8: Aachen Reg.Bez.
wk[1] = {'CDU':20509, 'SPD':8709, 'FDP':1842, 'KPD':3515, 'Z':2897, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[2] = {'CDU':18271, 'SPD':12002, 'FDP':811, 'KPD':5944, 'Z':1546, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[3] = {'CDU':18472, 'SPD':10916, 'FDP':831, 'KPD':4779, 'Z':1925, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[4] = {'CDU':20409, 'SPD':5650, 'FDP':472, 'KPD':1781, 'Z':1168, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[5] = {'CDU':14050, 'SPD':3970, 'FDP':473, 'KPD':1184, 'Z':705, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[6] = {'CDU':11361, 'SPD':5400, 'FDP':121, 'KPD':1485, 'Z':451, 'DRP':0, 'RWVP':184, 'Unab':0}
wk[7] = {'CDU':18754, 'SPD':11935, 'FDP':65, 'KPD':3000, 'Z':1096, 'DRP':0, 'RWVP':1994, 'Unab':0}
wk[8] = {'CDU':17021, 'SPD':4127, 'FDP':940, 'KPD':460, 'Z':1912, 'DRP':0, 'RWVP':0, 'Unab':0}

# WK 9-27: Köln Reg.Bez.
wk[9] = {'CDU':16924, 'SPD':8402, 'FDP':414, 'KPD':2093, 'Z':1710, 'DRP':0, 'RWVP':2522, 'Unab':0}
wk[10] = {'CDU':15737, 'SPD':11458, 'FDP':0, 'KPD':2922, 'Z':1225, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[11] = {'CDU':12896, 'SPD':10682, 'FDP':532, 'KPD':4076, 'Z':0, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[12] = {'CDU':10792, 'SPD':8473, 'FDP':537, 'KPD':3734, 'Z':739, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[13] = {'CDU':13478, 'SPD':10349, 'FDP':1577, 'KPD':6040, 'Z':374, 'DRP':0, 'RWVP':493, 'Unab':0}
wk[14] = {'CDU':17328, 'SPD':9125, 'FDP':2195, 'KPD':4464, 'Z':502, 'DRP':0, 'RWVP':371, 'Unab':0}
wk[15] = {'CDU':15160, 'SPD':10105, 'FDP':1286, 'KPD':6683, 'Z':534, 'DRP':0, 'RWVP':246, 'Unab':0}
wk[16] = {'CDU':16368, 'SPD':8953, 'FDP':1464, 'KPD':4897, 'Z':774, 'DRP':0, 'RWVP':306, 'Unab':0}
wk[17] = {'CDU':13330, 'SPD':9652, 'FDP':866, 'KPD':5935, 'Z':547, 'DRP':0, 'RWVP':164, 'Unab':0}
wk[18] = {'CDU':12453, 'SPD':8871, 'FDP':861, 'KPD':5839, 'Z':621, 'DRP':0, 'RWVP':280, 'Unab':0}
wk[19] = {'CDU':11334, 'SPD':6157, 'FDP':1689, 'KPD':1314, 'Z':2695, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[20] = {'CDU':14932, 'SPD':5973, 'FDP':696, 'KPD':950, 'Z':1252, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[21] = {'CDU':16519, 'SPD':8567, 'FDP':4612, 'KPD':2623, 'Z':2617, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[22] = {'CDU':15373, 'SPD':7225, 'FDP':1830, 'KPD':2295, 'Z':9476, 'DRP':0, 'RWVP':155, 'Unab':0}
wk[23] = {'CDU':14156, 'SPD':7502, 'FDP':3028, 'KPD':1868, 'Z':10361, 'DRP':0, 'RWVP':231, 'Unab':0}
wk[24] = {'CDU':9183, 'SPD':9892, 'FDP':2367, 'KPD':1402, 'Z':1706, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[25] = {'CDU':9789, 'SPD':7582, 'FDP':2613, 'KPD':1156, 'Z':1296, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[26] = {'CDU':15089, 'SPD':8001, 'FDP':1678, 'KPD':2774, 'Z':1474, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[27] = {'CDU':17549, 'SPD':6415, 'FDP':964, 'KPD':1607, 'Z':2136, 'DRP':0, 'RWVP':0, 'Unab':0}

# WK 28-77: Düsseldorf Reg.Bez.
wk[28] = {'CDU':11413, 'SPD':8075, 'FDP':899, 'KPD':2327, 'Z':2802, 'DRP':0, 'RWVP':256, 'Unab':0}
wk[29] = {'CDU':13714, 'SPD':9039, 'FDP':0, 'KPD':1884, 'Z':2759, 'DRP':0, 'RWVP':243, 'Unab':0}
wk[30] = {'CDU':10506, 'SPD':6222, 'FDP':1335, 'KPD':3093, 'Z':1305, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[31] = {'CDU':9895, 'SPD':6234, 'FDP':3361, 'KPD':3543, 'Z':5389, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[32] = {'CDU':9344, 'SPD':6639, 'FDP':1054, 'KPD':1872, 'Z':5617, 'DRP':0, 'RWVP':576, 'Unab':0}
wk[33] = {'CDU':10765, 'SPD':5688, 'FDP':2656, 'KPD':2685, 'Z':5460, 'DRP':0, 'RWVP':725, 'Unab':0}
wk[34] = {'CDU':12774, 'SPD':8292, 'FDP':1698, 'KPD':2131, 'Z':7419, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[35] = {'CDU':12879, 'SPD':9514, 'FDP':1820, 'KPD':2856, 'Z':6605, 'DRP':0, 'RWVP':208, 'Unab':0}
wk[36] = {'CDU':11036, 'SPD':9656, 'FDP':3164, 'KPD':3205, 'Z':3530, 'DRP':0, 'RWVP':334, 'Unab':0}
wk[37] = {'CDU':10371, 'SPD':10353, 'FDP':3085, 'KPD':3152, 'Z':2865, 'DRP':0, 'RWVP':289, 'Unab':0}
wk[38] = {'CDU':15322, 'SPD':5083, 'FDP':798, 'KPD':1006, 'Z':4222, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[39] = {'CDU':15894, 'SPD':5764, 'FDP':330, 'KPD':1393, 'Z':2046, 'DRP':0, 'RWVP':296, 'Unab':0}
wk[40] = {'CDU':12790, 'SPD':12598, 'FDP':1006, 'KPD':5399, 'Z':1413, 'DRP':848, 'RWVP':0, 'Unab':0}
wk[41] = {'CDU':10505, 'SPD':10675, 'FDP':2227, 'KPD':5789, 'Z':544, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[42] = {'CDU':11017, 'SPD':6500, 'FDP':509, 'KPD':1854, 'Z':3389, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[43] = {'CDU':14828, 'SPD':9020, 'FDP':2367, 'KPD':4861, 'Z':3645, 'DRP':0, 'RWVP':499, 'Unab':0}
wk[44] = {'CDU':14328, 'SPD':12092, 'FDP':1298, 'KPD':8098, 'Z':2232, 'DRP':0, 'RWVP':489, 'Unab':0}
wk[45] = {'CDU':14177, 'SPD':9603, 'FDP':2489, 'KPD':6166, 'Z':2899, 'DRP':0, 'RWVP':455, 'Unab':0}
wk[46] = {'CDU':9919, 'SPD':12227, 'FDP':788, 'KPD':9131, 'Z':3611, 'DRP':0, 'RWVP':318, 'Unab':0}
wk[47] = {'CDU':13154, 'SPD':10140, 'FDP':1444, 'KPD':5874, 'Z':3377, 'DRP':0, 'RWVP':352, 'Unab':0}
wk[48] = {'CDU':17798, 'SPD':13772, 'FDP':5515, 'KPD':7477, 'Z':2421, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[49] = {'CDU':15646, 'SPD':9741, 'FDP':7071, 'KPD':6530, 'Z':935, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[50] = {'CDU':11337, 'SPD':12186, 'FDP':7792, 'KPD':13292, 'Z':2537, 'DRP':1098, 'RWVP':0, 'Unab':0}
wk[51] = {'CDU':6521, 'SPD':8527, 'FDP':6245, 'KPD':9804, 'Z':512, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[52] = {'CDU':8225, 'SPD':8687, 'FDP':5561, 'KPD':8750, 'Z':856, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[53] = {'CDU':14073, 'SPD':13655, 'FDP':0, 'KPD':6425, 'Z':3554, 'DRP':2443, 'RWVP':380, 'Unab':0}
wk[54] = {'CDU':0, 'SPD':14055, 'FDP':11566, 'KPD':6809, 'Z':4531, 'DRP':2059, 'RWVP':407, 'Unab':0}
wk[55] = {'CDU':0, 'SPD':14261, 'FDP':13927, 'KPD':7185, 'Z':3763, 'DRP':2952, 'RWVP':417, 'Unab':0}
wk[56] = {'CDU':12462, 'SPD':12450, 'FDP':0, 'KPD':8201, 'Z':3078, 'DRP':2486, 'RWVP':225, 'Unab':0}
wk[57] = {'CDU':10459, 'SPD':10232, 'FDP':2918, 'KPD':4718, 'Z':2226, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[58] = {'CDU':8444, 'SPD':11356, 'FDP':4121, 'KPD':5832, 'Z':2247, 'DRP':1190, 'RWVP':0, 'Unab':0}
wk[59] = {'CDU':11550, 'SPD':10824, 'FDP':0, 'KPD':4593, 'Z':3341, 'DRP':1653, 'RWVP':0, 'Unab':0}
wk[60] = {'CDU':6616, 'SPD':12566, 'FDP':925, 'KPD':10397, 'Z':5046, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[61] = {'CDU':10578, 'SPD':12935, 'FDP':1546, 'KPD':8073, 'Z':5663, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[62] = {'CDU':9142, 'SPD':11105, 'FDP':755, 'KPD':8391, 'Z':5747, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[63] = {'CDU':8645, 'SPD':12118, 'FDP':970, 'KPD':8568, 'Z':4037, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[64] = {'CDU':9924, 'SPD':10413, 'FDP':2244, 'KPD':5304, 'Z':7955, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[65] = {'CDU':12505, 'SPD':9927, 'FDP':3734, 'KPD':4052, 'Z':4269, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[66] = {'CDU':11121, 'SPD':10184, 'FDP':2119, 'KPD':4350, 'Z':6636, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[67] = {'CDU':9373, 'SPD':8410, 'FDP':4605, 'KPD':4058, 'Z':828, 'DRP':928, 'RWVP':0, 'Unab':0}
wk[68] = {'CDU':7054, 'SPD':10483, 'FDP':2392, 'KPD':6182, 'Z':1331, 'DRP':599, 'RWVP':0, 'Unab':0}
wk[69] = {'CDU':8635, 'SPD':7831, 'FDP':1044, 'KPD':5486, 'Z':2849, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[70] = {'CDU':9035, 'SPD':8827, 'FDP':2167, 'KPD':5377, 'Z':3201, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[71] = {'CDU':8915, 'SPD':9765, 'FDP':1757, 'KPD':8216, 'Z':1940, 'DRP':0, 'RWVP':132, 'Unab':0}
wk[72] = {'CDU':9394, 'SPD':8780, 'FDP':2548, 'KPD':4702, 'Z':3044, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[73] = {'CDU':6817, 'SPD':10468, 'FDP':2089, 'KPD':10047, 'Z':3253, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[74] = {'CDU':10317, 'SPD':10917, 'FDP':1365, 'KPD':8582, 'Z':8101, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[75] = {'CDU':8517, 'SPD':10205, 'FDP':0, 'KPD':6782, 'Z':9646, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[76] = {'CDU':9011, 'SPD':10049, 'FDP':2239, 'KPD':5268, 'Z':4510, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[77] = {'CDU':14949, 'SPD':7040, 'FDP':1017, 'KPD':1879, 'Z':5250, 'DRP':0, 'RWVP':0, 'Unab':0}

# WK 78-98: Münster Reg.Bez.
wk[78] = {'CDU':16768, 'SPD':7959, 'FDP':710, 'KPD':2103, 'Z':9759, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[79] = {'CDU':13599, 'SPD':7338, 'FDP':695, 'KPD':1058, 'Z':9783, 'DRP':232, 'RWVP':0, 'Unab':0}
wk[80] = {'CDU':10880, 'SPD':7016, 'FDP':598, 'KPD':1569, 'Z':6445, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[81] = {'CDU':12819, 'SPD':7417, 'FDP':694, 'KPD':731, 'Z':7865, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[82] = {'CDU':10293, 'SPD':12301, 'FDP':4156, 'KPD':1107, 'Z':9434, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[83] = {'CDU':9056, 'SPD':3742, 'FDP':463, 'KPD':459, 'Z':8313, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[84] = {'CDU':10875, 'SPD':8324, 'FDP':612, 'KPD':4055, 'Z':2956, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[85] = {'CDU':14676, 'SPD':8191, 'FDP':735, 'KPD':974, 'Z':3715, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[86] = {'CDU':22883, 'SPD':11543, 'FDP':1659, 'KPD':4094, 'Z':6359, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[87] = {'CDU':10394, 'SPD':5154, 'FDP':1238, 'KPD':713, 'Z':11993, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[88] = {'CDU':11712, 'SPD':8768, 'FDP':2505, 'KPD':1621, 'Z':8873, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[89] = {'CDU':10086, 'SPD':5597, 'FDP':267, 'KPD':688, 'Z':11662, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[90] = {'CDU':12482, 'SPD':11109, 'FDP':562, 'KPD':4648, 'Z':4357, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[91] = {'CDU':11840, 'SPD':9476, 'FDP':581, 'KPD':6587, 'Z':3693, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[92] = {'CDU':10322, 'SPD':9210, 'FDP':756, 'KPD':7936, 'Z':3133, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[93] = {'CDU':13284, 'SPD':13395, 'FDP':1727, 'KPD':10436, 'Z':4387, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[94] = {'CDU':8735, 'SPD':11871, 'FDP':492, 'KPD':6942, 'Z':2003, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[95] = {'CDU':12089, 'SPD':11949, 'FDP':961, 'KPD':11364, 'Z':3960, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[96] = {'CDU':11628, 'SPD':13263, 'FDP':4485, 'KPD':10875, 'Z':3530, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[97] = {'CDU':9043, 'SPD':14342, 'FDP':2921, 'KPD':12166, 'Z':4769, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[98] = {'CDU':5987, 'SPD':14566, 'FDP':1876, 'KPD':10874, 'Z':10484, 'DRP':0, 'RWVP':0, 'Unab':0}

# WK 99-131: Arnsberg Reg.Bez.
wk[99] = {'CDU':12798, 'SPD':13237, 'FDP':1155, 'KPD':10913, 'Z':714, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[100] = {'CDU':14801, 'SPD':18164, 'FDP':2922, 'KPD':11934, 'Z':1342, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[101] = {'CDU':9766, 'SPD':10721, 'FDP':1112, 'KPD':8148, 'Z':993, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[102] = {'CDU':13288, 'SPD':12722, 'FDP':1459, 'KPD':9697, 'Z':2123, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[103] = {'CDU':12478, 'SPD':16155, 'FDP':1691, 'KPD':9698, 'Z':1178, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[104] = {'CDU':15021, 'SPD':12948, 'FDP':2165, 'KPD':8276, 'Z':1142, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[105] = {'CDU':11093, 'SPD':11414, 'FDP':0, 'KPD':6467, 'Z':731, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[106] = {'CDU':14488, 'SPD':13499, 'FDP':4195, 'KPD':7830, 'Z':1140, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[107] = {'CDU':12210, 'SPD':12992, 'FDP':2591, 'KPD':11157, 'Z':1459, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[108] = {'CDU':12987, 'SPD':17554, 'FDP':2590, 'KPD':8718, 'Z':920, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[109] = {'CDU':11353, 'SPD':16395, 'FDP':1724, 'KPD':9558, 'Z':796, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[110] = {'CDU':10778, 'SPD':14896, 'FDP':2034, 'KPD':9655, 'Z':2414, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[111] = {'CDU':11602, 'SPD':16459, 'FDP':2077, 'KPD':8507, 'Z':1165, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[112] = {'CDU':11629, 'SPD':19454, 'FDP':1879, 'KPD':7446, 'Z':1951, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[113] = {'CDU':13094, 'SPD':17922, 'FDP':1686, 'KPD':4450, 'Z':1552, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[114] = {'CDU':7349, 'SPD':8183, 'FDP':2722, 'KPD':2050, 'Z':3989, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[115] = {'CDU':16385, 'SPD':10923, 'FDP':1826, 'KPD':1341, 'Z':8992, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[116] = {'CDU':19554, 'SPD':9812, 'FDP':0, 'KPD':1375, 'Z':2746, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[117] = {'CDU':22307, 'SPD':15640, 'FDP':603, 'KPD':1809, 'Z':6818, 'DRP':0, 'RWVP':0, 'Unab':222}
wk[118] = {'CDU':13992, 'SPD':16903, 'FDP':2674, 'KPD':5013, 'Z':2550, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[119] = {'CDU':15295, 'SPD':15382, 'FDP':3060, 'KPD':3261, 'Z':7892, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[120] = {'CDU':9374, 'SPD':11397, 'FDP':4888, 'KPD':6925, 'Z':324, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[121] = {'CDU':11729, 'SPD':10506, 'FDP':5155, 'KPD':5656, 'Z':363, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[122] = {'CDU':10423, 'SPD':15540, 'FDP':1639, 'KPD':6707, 'Z':467, 'DRP':1847, 'RWVP':0, 'Unab':0}
wk[123] = {'CDU':11996, 'SPD':15995, 'FDP':9093, 'KPD':9080, 'Z':511, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[124] = {'CDU':13617, 'SPD':20139, 'FDP':6828, 'KPD':8815, 'Z':773, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[125] = {'CDU':15022, 'SPD':17180, 'FDP':2203, 'KPD':3778, 'Z':1532, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[126] = {'CDU':18167, 'SPD':18742, 'FDP':2335, 'KPD':4757, 'Z':0, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[127] = {'CDU':18865, 'SPD':8336, 'FDP':1361, 'KPD':1349, 'Z':4807, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[128] = {'CDU':14330, 'SPD':12419, 'FDP':2760, 'KPD':3198, 'Z':0, 'DRP':3113, 'RWVP':0, 'Unab':0}
wk[129] = {'CDU':13277, 'SPD':10433, 'FDP':1387, 'KPD':1250, 'Z':0, 'DRP':3431, 'RWVP':0, 'Unab':0}
wk[130] = {'CDU':24423, 'SPD':14523, 'FDP':0, 'KPD':1719, 'Z':3525, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[131] = {'CDU':14302, 'SPD':7066, 'FDP':624, 'KPD':574, 'Z':6393, 'DRP':0, 'RWVP':0, 'Unab':0}

# WK 132-150: Detmold Reg.Bez.
wk[132] = {'CDU':11946, 'SPD':3828, 'FDP':0, 'KPD':479, 'Z':7345, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[133] = {'CDU':8746, 'SPD':4021, 'FDP':925, 'KPD':290, 'Z':7039, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[134] = {'CDU':18016, 'SPD':7637, 'FDP':614, 'KPD':1435, 'Z':9375, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[135] = {'CDU':16420, 'SPD':6519, 'FDP':591, 'KPD':681, 'Z':10230, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[136] = {'CDU':22951, 'SPD':10313, 'FDP':2099, 'KPD':1251, 'Z':6439, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[137] = {'CDU':16458, 'SPD':21172, 'FDP':1114, 'KPD':3291, 'Z':855, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[138] = {'CDU':10516, 'SPD':14317, 'FDP':2776, 'KPD':2228, 'Z':956, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[139] = {'CDU':10006, 'SPD':14781, 'FDP':2564, 'KPD':3123, 'Z':695, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[140] = {'CDU':9159, 'SPD':8317, 'FDP':951, 'KPD':954, 'Z':191, 'DRP':0, 'RWVP':0, 'Unab':633}
wk[141] = {'CDU':11581, 'SPD':16760, 'FDP':1547, 'KPD':1707, 'Z':281, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[142] = {'CDU':10136, 'SPD':10912, 'FDP':1420, 'KPD':1703, 'Z':0, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[143] = {'CDU':11686, 'SPD':15440, 'FDP':1354, 'KPD':2420, 'Z':308, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[144] = {'CDU':13954, 'SPD':10122, 'FDP':1132, 'KPD':1061, 'Z':157, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[145] = {'CDU':15090, 'SPD':12257, 'FDP':1636, 'KPD':3012, 'Z':128, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[146] = {'CDU':13129, 'SPD':17084, 'FDP':937, 'KPD':2467, 'Z':108, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[147] = {'CDU':11940, 'SPD':10206, 'FDP':2381, 'KPD':2129, 'Z':337, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[148] = {'CDU':13088, 'SPD':12569, 'FDP':812, 'KPD':1996, 'Z':486, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[149] = {'CDU':9796, 'SPD':10758, 'FDP':1286, 'KPD':1927, 'Z':644, 'DRP':0, 'RWVP':0, 'Unab':0}
wk[150] = {'CDU':8792, 'SPD':13016, 'FDP':2400, 'KPD':1678, 'Z':609, 'DRP':0, 'RWVP':0, 'Unab':0}

# --- WK-to-Kreis mapping (same 150 WK structure as 1950) ---
WK_TO_KREIS = {
    1: ("Aachen", "krfr"), 2: ("Kreis Aachen", "kreis"), 3: ("Kreis Aachen", "kreis"),
    4: ("Selfkantkreis Geilenkirchen-Heinsberg", "kreis"), 5: ("Kreis Erkelenz", "kreis"),
    6: ("Kreis Jülich + Kreis Düren", "combined"), 7: ("Kreis Jülich + Kreis Düren", "combined"),
    8: ("Kreis Monschau + Kreis Schleiden", "combined"),
    9: ("Kreis Euskirchen", "kreis"), 10: ("Kreis Bergheim (Erft)", "kreis"),
    11: ("Kreis Köln", "kreis"), 12: ("Kreis Köln", "kreis"),
    13: ("Köln", "krfr"), 14: ("Köln", "krfr"), 15: ("Köln", "krfr"),
    16: ("Köln", "krfr"), 17: ("Köln", "krfr"), 18: ("Köln", "krfr"),
    19: ("Kreis Bonn", "kreis"), 20: ("Kreis Bonn", "kreis"),
    21: ("Bonn", "krfr"), 22: ("Siegkreis", "kreis"), 23: ("Siegkreis", "kreis"),
    24: ("Oberbergischer Kreis", "kreis"), 25: ("Oberbergischer Kreis", "kreis"),
    26: ("Rheinisch-Bergischer Kreis", "kreis"), 27: ("Rheinisch-Bergischer Kreis", "kreis"),
    28: ("Kreis Grevenbroich", "kreis"), 29: ("Kreis Grevenbroich", "kreis"),
    30: ("Neuss", "krfr"), 31: ("Rheydt", "krfr"),
    32: ("Mönchengladbach + Viersen", "combined"), 33: ("Mönchengladbach + Viersen", "combined"),
    34: ("Kreis Kempen-Krefeld", "kreis"), 35: ("Kreis Kempen-Krefeld", "kreis"),
    36: ("Krefeld", "krfr"), 37: ("Krefeld", "krfr"),
    38: ("Kreis Geldern", "kreis"), 39: ("Kreis Kleve", "kreis"),
    40: ("Kreis Moers", "kreis"), 41: ("Kreis Moers", "kreis"), 42: ("Kreis Moers", "kreis"),
    43: ("Düsseldorf", "krfr"), 44: ("Düsseldorf", "krfr"),
    45: ("Düsseldorf", "krfr"), 46: ("Düsseldorf", "krfr"), 47: ("Düsseldorf", "krfr"),
    48: ("Rhein-Wupper-Kreis", "kreis"), 49: ("Rhein-Wupper-Kreis", "kreis"),
    50: ("Remscheid", "krfr"), 51: ("Solingen", "krfr"), 52: ("Solingen", "krfr"),
    53: ("Wuppertal", "krfr"), 54: ("Wuppertal", "krfr"),
    55: ("Wuppertal", "krfr"), 56: ("Wuppertal", "krfr"),
    57: ("Kreis Düsseldorf-Mettmann", "kreis"), 58: ("Kreis Düsseldorf-Mettmann", "kreis"),
    59: ("Kreis Düsseldorf-Mettmann", "kreis"),
    60: ("Essen", "krfr"), 61: ("Essen", "krfr"), 62: ("Essen", "krfr"),
    63: ("Essen", "krfr"), 64: ("Essen", "krfr"), 65: ("Essen", "krfr"), 66: ("Essen", "krfr"),
    67: ("Mülheim a.d. Ruhr", "krfr"), 68: ("Mülheim a.d. Ruhr", "krfr"),
    69: ("Duisburg", "krfr"), 70: ("Duisburg", "krfr"), 71: ("Duisburg", "krfr"),
    72: ("Duisburg", "krfr"), 73: ("Duisburg", "krfr"),
    74: ("Oberhausen", "krfr"), 75: ("Oberhausen", "krfr"),
    76: ("Kreis Dinslaken", "kreis"), 77: ("Kreis Rees", "kreis"),
    78: ("Kreis Borken", "kreis"), 79: ("Kreis Ahaus", "kreis"),
    80: ("Kreis Steinfurt", "kreis"), 81: ("Kreis Steinfurt", "kreis"),
    82: ("Kreis Tecklenburg", "kreis"), 83: ("Kreis Warendorf", "kreis"),
    84: ("Kreis Beckum", "kreis"), 85: ("Kreis Beckum", "kreis"),
    86: ("Kreis Lüdinghausen", "kreis"), 87: ("Kreis Münster", "kreis"),
    88: ("Münster", "krfr"), 89: ("Kreis Coesfeld", "kreis"),
    90: ("Kreis Recklinghausen", "kreis"), 91: ("Kreis Recklinghausen", "kreis"),
    92: ("Kreis Recklinghausen", "kreis"),
    93: ("Recklinghausen", "krfr"), 94: ("Gladbeck", "krfr"),
    95: ("Bottrop", "krfr"), 96: ("Gelsenkirchen", "krfr"),
    97: ("Gelsenkirchen", "krfr"), 98: ("Gelsenkirchen", "krfr"),
    99: ("Wanne-Eickel", "krfr"), 100: ("Herne", "krfr"),
    101: ("Wattenscheid", "krfr"), 102: ("Bochum", "krfr"),
    103: ("Bochum", "krfr"), 104: ("Bochum", "krfr"),
    105: ("Castrop-Rauxel", "krfr"),
    106: ("Dortmund + Lünen", "combined"), 107: ("Dortmund + Lünen", "combined"),
    108: ("Dortmund + Lünen", "combined"), 109: ("Dortmund + Lünen", "combined"),
    110: ("Dortmund + Lünen", "combined"), 111: ("Dortmund + Lünen", "combined"),
    112: ("Kreis Unna", "kreis"), 113: ("Kreis Unna", "kreis"),
    114: ("Hamm", "krfr"), 115: ("Kreis Soest", "kreis"),
    116: ("Kreis Lippstadt", "kreis"), 117: ("Kreis Arnsberg", "kreis"),
    118: ("Iserlohn + Kreis Iserlohn", "combined"),
    119: ("Iserlohn + Kreis Iserlohn", "combined"),
    120: ("Hagen", "krfr"), 121: ("Hagen", "krfr"),
    122: ("Witten", "krfr"), 123: ("Ennepe-Ruhr-Kreis", "kreis"),
    124: ("Ennepe-Ruhr-Kreis", "kreis"),
    125: ("Lüdenscheid + Kreis Altena", "combined"),
    126: ("Lüdenscheid + Kreis Altena", "combined"),
    127: ("Kreis Olpe", "kreis"),
    128: ("Siegen + Kreis Siegen", "combined"), 129: ("Siegen + Kreis Siegen", "combined"),
    130: ("Kreis Meschede + Kreis Wittgenstein", "combined"),
    131: ("Kreis Brilon", "kreis"),
    132: ("Kreis Büren", "kreis"), 133: ("Kreis Warburg", "kreis"),
    134: ("Kreis Höxter", "kreis"), 135: ("Kreis Paderborn", "kreis"),
    136: ("Kreis Wiedenbrück", "kreis"),
    137: ("Kreis Bielefeld", "kreis"),
    138: ("Bielefeld", "krfr"), 139: ("Bielefeld", "krfr"),
    140: ("Kreis Halle (Westf.)", "kreis"),
    141: ("Herford + Kreis Herford", "combined"),
    142: ("Herford + Kreis Herford", "combined"),
    143: ("Herford + Kreis Herford", "combined"),
    144: ("Kreis Lübbecke", "kreis"),
    145: ("Kreis Minden", "kreis"), 146: ("Kreis Minden", "kreis"),
    147: ("Kreis Detmold", "kreis"), 148: ("Kreis Detmold", "kreis"),
    149: ("Kreis Lemgo", "kreis"), 150: ("Kreis Lemgo", "kreis"),
}

PARTIES_1947 = ['CDU', 'SPD', 'FDP', 'KPD', 'Z', 'DRP', 'RWVP', 'Unab']
PARTY_CSV = ['cdu', 'spd', 'fdp', 'kpd', 'zentrum', 'drp', 'rwvp', 'unabhaengige']

def aggregate_to_kreis():
    kreis_data = {}
    for wk_num in sorted(wk):
        k, t = WK_TO_KREIS[wk_num]
        key = (k, t)
        if key not in kreis_data:
            kreis_data[key] = {p: 0 for p in PARTIES_1947}
        for p in PARTIES_1947:
            kreis_data[key][p] += wk[wk_num].get(p, 0)
    return kreis_data

def output_csv(kreis_data, path):
    hdr = ['name', 'type', 'election_year', 'valid_votes'] + PARTY_CSV
    rows = []
    for (name, typ), parties in sorted(kreis_data.items()):
        vv = sum(parties.values())
        row = [name, typ if typ != 'combined' else 'combined', 1947, vv]
        for p in PARTIES_1947:
            v = parties[p]
            row.append(v if v > 0 else '')
        rows.append(row)
    with open(path, 'w', newline='', encoding='utf-8') as f:
        w = csv.writer(f)
        w.writerow(hdr)
        w.writerows(rows)
    print(f"Wrote {len(rows)} rows to {path}")

if __name__ == '__main__':
    kd = aggregate_to_kreis()
    print(f"150 WK -> {len(kd)} Kreise")

    # Statewide check
    total = {p: sum(wk[i][p] for i in wk) for p in PARTIES_1947}
    print(f"Statewide: {sum(total.values()):,} gültige")
    for p in PARTIES_1947:
        print(f"  {p}: {total[p]:,} ({total[p]/sum(total.values())*100:.1f}%)")

    out = os.path.join(os.path.dirname(__file__), '..', '..', 'data', 'state_elections', 'raw',
                       'Landtagswahlen', 'Nordrhein-Westfalen', 'nrw_1947_kreis.csv')
    output_csv(kd, out)
