bit0:

y00 XOR x00 -> z00
x00 AND y00 -> gjk -- carry0

bit1:

y01 XOR x01 -> cbs -- value sans carry
x01 AND y01 -> dvp --

gjk XOR cbs -> z01
cbs AND gjk -> pct

dvp OR pct -> sbw -- carry1

bit2:

x02 XOR y02 -> rbr
y02 AND x02 -> bwb

rbr XOR sbw -> z02
rbr AND sbw -> nng

nng OR bwb -> trb -- carry2

---

ERRORS:

hnv,hth,kfm,tqr,vmv,z07,z20,z28

---

hth, tqr

x35 XOR y35 -> hth (*)
x35 AND y35 -> tqr (*)

tqr XOR vkc -> z35
vkc AND tqr -> chh

hth OR chh -> dkj

---

hnv, z28

y28 XOR x28 -> vwh
x28 AND y28 -> hcr

jsg XOR vwh -> hnv (*)
vwh AND jsg -> z28 (*)

hcr OR hnv -> std

---

kfm, z20

x20 XOR y20 -> bqn
y20 AND x20 -> hds

smh XOR bqn -> kfm (*)
smh AND bqn -> jbq

jbq OR hds -> z20 (*)

---

vmv, z07

y07 XOR x07 -> pkm
y07 AND x07 -> z07 (*)

pkm XOR cvq -> vmv (*)
pkm AND cvq -> kbg

vmv OR kbg -> hkj

---

bit44:

y44 XOR x44 -> qcp
x44 AND y44 -> bvd

hpj XOR qcp -> z44
hpj AND qcp -> vfw

vfw OR bvd -> z45 -- carry44