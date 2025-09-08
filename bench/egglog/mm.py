import os

# Read file mm.py which is eqsat on 3mm.egg like this:
# (let X (Mat 200 175))
# (let Y (Mat 175 250))
# (let Z (Mat 250 150))
# (let W (Mat 150 10))
#
# (let XY (Mul X Y))
# (let XY_Z (Mul XY Z))
# (let XY_Z__W (Mul XY_Z W))


# Replace lines 7 to 14 included with NMM and call the file nmm.egg

def gen_nmm(n):
    """Generate the let statements for NMM, where all matrices are 100x100"""
    lets = [f'(let m{i} (Mat 100 100))\n' for i in range(n + 1)]
    mms = ["(let mm0 (Mul m0 m1))\n"] + [f'(let mm{i+1} (Mul mm{i} m{i+2}))\n' for i in range(n - 1)]
    return lets + mms

print(gen_nmm(10))

egg_file = 'mm.egg'

for n in [3, 5, 10, 20, 40, 80]:
    nmm_file = f'{n}mm.egg'
    nmm = gen_nmm(n)

    with open(egg_file, 'r') as f:
        lines = f.readlines()

    with open(nmm_file, 'w') as f:
        f.writelines(lines[:6] + nmm + lines[14:-1])
        f.write(f'(extract mm{n-1})\n')

    print(f'Generated {nmm_file}')