'ULTIMA293.BAS is a software application that enables the user to 
'program their computer to assemble total-dependent-basic-strategy tables
'for card games belonging to the Blackjack family of table-card games.

'Copyright (C) 2016  Kevin Michael McDaniel

'This program is free software: you can redistribute it and/or modify
'it under the terms of the GNU General Public License as published by
'the Free Software Foundation, either version 3 of the License, or
'(at your option) any later version.

'This program is distributed in the hope that it will be useful,
'but WITHOUT ANY WARRANTY; without even the implied warranty of
'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'GNU General Public License for more details.

'You should have received a copy of the GNU General Public License
'along with this program.  If not, see <http://www.gnu.org/licenses/>.

_TITLE "ULTIMA293.BAS"
TYPE bsrules
    c1 AS INTEGER
    c2 AS INTEGER
    c3 AS INTEGER
    c4 AS INTEGER
    c5 AS INTEGER
    c6 AS INTEGER
    c7 AS INTEGER
    c8 AS INTEGER
    c9 AS INTEGER
    cT AS INTEGER
    cJ AS INTEGER
    cQ AS INTEGER
    cK AS INTEGER
    j AS INTEGER
    dwt AS INTEGER
    dbc AS INTEGER
    hole AS INTEGER
    push AS INTEGER
    ten AS INTEGER
    schw AS INTEGER
    ls AS INTEGER
    early AS INTEGER
    splt AS INTEGER
    nsh AS INTEGER
    rsa AS INTEGER
    das AS INTEGER
    dh AS INTEGER
    ld AS INTEGER
    blackjack AS SINGLE
    ins AS INTEGER
    dsa AS INTEGER
    bjsh AS INTEGER
    deck AS INTEGER
    shf AS INTEGER
    immed AS INTEGER
    pbd AS INTEGER
    b21po AS INTEGER
    ms777 AS SINGLE
    ss777 AS SINGLE
    sp777 AS SINGLE
    jp777 AS SINGLE
    po5c21 AS SINGLE
    po6c21 AS SINGLE
    po7c21 AS SINGLE
    po8c21 AS SINGLE
    ms678 AS SINGLE
    ss678 AS SINGLE
    sp678 AS SINGLE
    dd AS INTEGER
    dd3 AS INTEGER
    dd4 AS INTEGER
    dd5 AS INTEGER
    dd6 AS INTEGER
    dd7 AS INTEGER
    description AS STRING * 80
END TYPE
TYPE bs
    far AS INTEGER
    sar AS INTEGER
    r AS INTEGER
    hit AS SINGLE
END TYPE
TYPE xbase
    dat AS SINGLE
END TYPE
DIM database AS xbase: DIM sd AS bsrules: DIM num AS bs: DIM g(32) AS INTEGER
DIM h(32) AS INTEGER: DIM mc(22): DIM card(22) AS INTEGER: DIM hr(65, 10) AS INTEGER
DIM hit(54, 9): DIM sar(54, 9) AS INTEGER: DIM far(54, 9) AS INTEGER
DIM br(54, 9) AS INTEGER: DIM aq(54, 9, 32): DIM bc(12) AS INTEGER
DIM ss(9, 7) AS INTEGER: DIM hh(9, 15) AS INTEGER: DIM hp(9, 9) AS INTEGER
DIM prob(54, 9): DIM htotal(9, 32): DIM stotal(9, 32): DIM dd(3 TO 7) AS INTEGER
DIM mc21po(11) AS SINGLE: DIM m(9, 33) AS INTEGER: DIM pfr(36) AS INTEGER
DIM psr(36) AS INTEGER: DIM i(34) AS INTEGER: DIM psum(3, 36)
DIM hf(9) AS INTEGER: DIM sf(9) AS INTEGER
DIM rc(9) AS INTEGER: DIM sprob(9, 32): DIM hprob(9, 32): DIM pop(5) AS SINGLE
CLS:
PRINT "ULTIMA293.BAS prompts the user to enter the folder/filename prefix to be used"
PRINT "to save application output. ULTIMA293.BAS prompts the user to enter the"
PRINT "folder/filename prefix of the set of files to be loaded into program memory."
PRINT "The set of files contains data. The data consists of data about a user-defined"
PRINT "set of game rules for a card game belonging to the Blackjack family of"
PRINT "table-card games; and data about a set of tables assembled for the card game."
PRINT "The set of tables consists of a set of combination-dependent-playing-strategy"
PRINT "tables and a combination-dependent-expected-value table. ULTIMA282.BAS and"
PRINT "BJ282.BAS are both capable of both generating the data and saving the set of"
PRINT "files. ULTIMA293.BAS uses the data to assemble a total-dependent-basic-strategy"
PRINT "table for the card game. ULTIMA293.BAS saves a set of files. The set of files"
PRINT "contains data. The data consists of data about the user-defined set of game"
PRINT "rules; and data about the assembled total-dependent-basic-strategy table."
PRINT " "
69 PRINT "enter the folder/filename prefix of the files to be used":
PRINT "to store application output": INPUT files$
PRINT " "
PRINT "enter the folder/filename prefix of the set of files"
PRINT "to be loaded into program memory": INPUT D$
v$ = D$ + "a.dat"
OPEN v$ FOR RANDOM AS #1 LEN = LEN(sd)
GET #1, , sd
bc(0) = sd.c1: bc(1) = sd.c2: bc(2) = sd.c3: bc(3) = sd.c4: bc(4) = sd.c5
bc(5) = sd.c6: bc(6) = sd.c7: bc(7) = sd.c8: bc(8) = sd.c9: bc(9) = sd.cT
bc(10) = sd.cJ: bc(11) = sd.cQ: bc(12) = sd.cK: dwt% = sd.dwt: dbc% = sd.dbc
j = sd.j: hole% = sd.hole: schw% = sd.schw: blackjack = sd.blackjack: ins% = sd.ins
splt% = sd.splt: most% = sd.nsh: rsa% = sd.rsa: das% = sd.das: dh% = sd.dh
ld% = sd.ld: ls% = sd.ls: early% = sd.early: bjsh% = sd.bjsh: dsa% = sd.dsa
push% = sd.push: ten% = sd.ten: deck% = sd.deck: shf = sd.shf: immed% = sd.immed: pbd% = sd.pbd
b21po% = sd.b21po: ms777 = sd.ms777: ss777 = sd.ss777: sp777 = sd.sp777: jp777 = sd.jp777
mc21po(4) = sd.po5c21: mc21po(5) = sd.po6c21: mc21po(6) = sd.po7c21: mc21po(7) = sd.po8c21
ms678 = sd.ms678: ss678 = sd.ss678: sp678 = sd.sp678
t$ = sd.description
CLOSE #1
ERASE card
FOR x = 0 TO 12
    IF x < 9 THEN card(x) = bc(x) ELSE card(9) = card(9) + bc(x)
NEXT x
card(10) = card(0) + card(1) + card(2) + card(3) + card(4) + card(5) + card(6) + card(7) + card(8) + card(9)
o$ = "     12/A612/A712/A812/A913/A613/A713/A813/A914/A614/A714/A814/A915/A615/A715/A815/A916/A616/A716/A816/A917/A617/A717/A817/A918/A718/A818/A919/A819/A9SplitSrndrDDown"
n$ = "     12   12   12   12   13   13   13   13   14   14   14   14   15   15   15   15   16   16   16   16   17   17   17   17   18   18   18   19   19   SplitSrndrDDown"
mc21po(1) = (4 * deck%) * (4 * deck% - 1) * (4 * deck% - 2): mc21po(2) = deck% * (deck% - 1) * (deck% - 2) * 4 / mc21po(1)
mc21po(3) = deck% * (deck% - 1) * (deck% - 2) / mc21po(1): mc21po(1) = 1: mc21po(8) = (4 * deck%) ^ 3 * 6
mc21po(9) = (deck% * 3) * (deck% * 2) * (deck% * 1) * 4 / mc21po(8): mc21po(10) = (deck% * 3) * (deck% * 2) * (deck% * 1) / mc21po(8): mc21po(8) = 1
FOR x = 0 TO 10
mc(x) = card(x) * deck%: NEXT x
v$ = D$ + "b.dat"
OPEN v$ FOR RANDOM AS #1 LEN = LEN(num)
w = 0
FOR x = 0 TO 54
    FOR y = 0 TO 9
        w = w + 1
        GET #1, w, num
        br(x, y) = num.r: far(x, y) = num.far: sar(x, y) = num.sar: hit(x, y) = num.hit
NEXT y: NEXT x
CLOSE #1
v$ = D$ + "t.dat"
OPEN v$ FOR RANDOM AS #1 LEN = LEN(database)
w = 0
FOR x = 0 TO 54
    FOR y = 0 TO 9
        FOR z = 0 TO 32
            w = w + 1
            GET #1, w, database
            aq(x, y, z) = database.dat
NEXT z: NEXT y: NEXT x
CLOSE #1
PRINT "the data loaded is described as follows:"; t$
PRINT "if this is acceptable type 'y'and press enter";: INPUT ans$: IF ans$ <> "y" THEN GOTO 69
PRINT "press 1 and enter to manually input the hard score and soft score"
PRINT "the player stands at while playing against each dealer up card ";: INPUT skip%
v$ = files$ + "i.txt"
OPEN v$ FOR OUTPUT AS #1
PRINT #1, "ULTIMA293.BAS prompts the user to enter the folder/filename prefix to be used"
PRINT #1, "to save application output. ULTIMA293.BAS prompts the user to enter the"
PRINT #1, "folder/filename prefix of the set of files to be loaded into program memory."
PRINT #1, "The set of files contains data. The data consists of data about a user-defined"
PRINT #1, "set of game rules for a card game belonging to the Blackjack family of"
PRINT #1, "table-card games; and data about a set of tables assembled for the card game."
PRINT #1, "The set of tables consists of a set of combination-dependent-playing-strategy"
PRINT #1, "tables and a combination-dependent-expected-value table. ULTIMA282.BAS and"
PRINT #1, "BJ282.BAS are both capable of both generating the data and saving the set of"
PRINT #1, "files. ULTIMA293.BAS uses the data to assemble a total-dependent-basic-strategy"
PRINT #1, "table for the card game. ULTIMA293.BAS saves a set of files. The set of files"
PRINT #1, "contains data. The data consists of data about the user-defined set of game"
PRINT #1, "rules; and data about the assembled total-dependent-basic-strategy table."
PRINT #1, " "
PRINT #1, "enter the folder/filename prefix of the files to be used":
PRINT #1, "to store application output": PRINT #1, files$
PRINT #1, " "
PRINT #1, "enter the folder/filename prefix of the set of files"
PRINT #1, "to be loaded into program memory": PRINT #1, D$
PRINT #1, "the data loaded is described as follows: "; t$
PRINT #1, "if this is acceptable type 'y'and press enter ";: PRINT #1, ans$
PRINT #1, "press 1 and enter to manually input the hard score and soft score"
PRINT #1, "the player stands at while playing against each dealer up card "; skip%
CLOSE #1
z = 0
FOR x = 12 TO 19
    FOR y = 17 TO 20
        IF x > y THEN y = x
        z = z + 1
        g(z) = x: h(z) = y
NEXT y: NEXT x: g(30) = 30: g(31) = 31: g(32) = 32: h(30) = 30: h(31) = 31: h(32) = 32
FOR k = 0 TO 9
    FOR l = k TO 9
        n = k / 2 * (19 - k) + l
        FOR m = 0 TO 9
            pc1 = k: pc2 = l: dc1 = m
            FOR x = 0 TO 10
            mc(x) = card(x) * deck%: NEXT x
            IF mc(pc1) < 1 THEN mc(11) = mc(pc1) ELSE mc(11) = 1
            IF mc(pc1) = 0 THEN GOTO 567 ELSE prob(n, dc1) = mc(pc1) / mc(10): mc(pc1) = mc(pc1) - mc(11): mc(10) = mc(10) - mc(11)
            IF mc(pc2) < 1 THEN mc(11) = mc(pc2) ELSE mc(11) = 1
            IF mc(pc2) = 0 THEN GOTO 567 ELSE prob(n, dc1) = prob(n, dc1) * mc(pc2) / mc(10): mc(pc2) = mc(pc2) - mc(11): mc(10) = mc(10) - mc(11)
            IF pc1 <> pc2 THEN prob(n, dc1) = prob(n, dc1) * 2
            IF mc(dc1) < 1 THEN mc(11) = mc(dc1) ELSE mc(11) = 1
            IF mc(dc1) = 0 THEN GOTO 567 ELSE prob(n, dc1) = prob(n, dc1) * mc(dc1) / mc(10): mc(dc1) = mc(dc1) - mc(11): mc(10) = mc(10) - mc(11)
            probs# = probs# + prob(n, dc1)
            IF br(n, m) < 30 THEN
                IF k * l <> 0 THEN IF g(br(n, m)) > k + l + 2 THEN pop(2) = pop(2) + prob(n, dc1) ELSE pop(1) = pop(1) + prob(n, dc1)
                IF k * l = 0 THEN IF h(br(n, m)) > k + l + 12 THEN pop(2) = pop(2) + prob(n, dc1) ELSE pop(1) = pop(1) + prob(n, dc1)
            END IF
            IF br(n, m) = 32 THEN pop(3) = pop(3) + prob(n, dc1)
            IF br(n, m) = 30 THEN pop(4) = pop(4) + prob(n, dc1)
            IF br(n, m) = 31 THEN pop(5) = pop(5) + prob(n, dc1)
567 NEXT m: NEXT l: NEXT k
IF skip% = 1 THEN
    FOR x = 0 TO 9
        IF x = 0 THEN
            PRINT "input the hard score the player stands at against a dealer Ace ";: INPUT hf(x)
            PRINT "input the soft score the player stands at against a dealer Ace ";: INPUT sf(x)
            OPEN v$ FOR APPEND AS #1
            PRINT #1, "input the hard score the player stands at against a dealer Ace "; hf(x)
            PRINT #1, "input the soft score the player stands at against a dealer Ace "; sf(x)
            CLOSE #1
        END IF
        IF x <> 0 THEN
            PRINT "input the hard score the player stands at against a dealer "; x + 1; " ";: INPUT hf(x)
            PRINT "input the soft score the player stands at against a dealer "; x + 1; " ";: INPUT sf(x)
            OPEN v$ FOR APPEND AS #1
            PRINT #1, "input the hard score the player stands at against a dealer "; x + 1; " "; hf(x)
            PRINT #1, "input the soft score the player stands at against a dealer "; x + 1; " "; sf(x)
            CLOSE #1
        END IF
        z = 0
        FOR h = 12 TO 19
            FOR s = 17 TO 20
                IF h > s THEN s = h
                z = z + 1
                IF h = hf(x) THEN
                    IF s = sf(x) THEN
                        rc(x) = z
                    END IF
                END IF
    NEXT s: NEXT h: NEXT x
END IF
IF skip% <> 1 THEN
    100 REM figure player's hard and soft stand at totals for each dealer up card
    FOR dc1 = 0 TO 9
        FOR pc1 = 1 TO 9
            FOR pc2 = pc1 TO 9
                n = pc1 / 2 * (19 - pc1) + pc2
                IF br(n, dc1) < 30 AND pc1 <> pc2 THEN
                    FOR op = 1 TO 29
                        IF pc1 + pc2 + 2 < g(br(n, dc1)) AND g(br(n, dc1)) < 30 THEN
                            htotal(dc1, op) = htotal(dc1, op) + aq(n, dc1, op) * prob(n, dc1): hprob(dc1, op) = hprob(dc1, op) + prob(n, dc1)
                        END IF
                    NEXT op
                END IF
        NEXT pc2: NEXT pc1
        FOR pc1 = 0 TO 9
            FOR pc2 = pc1 TO 9
                n = pc1 / 2 * (19 - pc1) + pc2
                q = -2
                FOR op = 1 TO 29
                    IF aq(n, dc1, op) > q THEN q = aq(n, dc1, op): hr(n, dc1) = op
                NEXT op
                IF br(n, dc1) < 30 AND pc1 <> pc2 THEN
                    IF pc1 + pc2 + 12 < h(br(n, dc1)) THEN
                        IF h(br(n, dc1)) < 30 THEN
                            FOR op = 1 TO 29
                                IF pc1 = 0 THEN
                                    stotal(dc1, op) = stotal(dc1, op) + aq(n, dc1, op) * prob(n, dc1)
                                    sprob(dc1, op) = sprob(dc1, op) + prob(n, dc1)
                                END IF
                                IF pc1 <> 0 THEN
                                    stotal(dc1, op) = stotal(dc1, op) + aq(n, dc1, op) * prob(n, dc1) * card(0) / card(10)
                                    sprob(dc1, op) = sprob(dc1, op) + prob(n, dc1) * card(0) / card(10)
                                END IF
                            NEXT op
                        END IF
                    END IF
                END IF
        NEXT pc2: NEXT pc1
        best! = -3
        FOR op = 1 TO 29
            IF stotal(dc1, op) / sprob(dc1, op) > best! THEN
                best! = stotal(dc1, op) / sprob(dc1, op)
                sf(dc1) = h(op)
            END IF
        NEXT op
        best! = -3
        FOR op = 1 TO 29
            IF htotal(dc1, op) / hprob(dc1, op) > best! THEN
                best! = htotal(dc1, op) / hprob(dc1, op)
                hf(dc1) = g(op)
            END IF
        NEXT op
        z = 0
        FOR x = 12 TO 19
            FOR y = 17 TO 20
                IF x > y THEN y = x
                z = z + 1
                IF hf(dc1) = g(z) AND sf(dc1) = h(z) THEN rc(dc1) = z
        NEXT y: NEXT x
    NEXT dc1
END IF
200 REM hp(9, 9) stores hard hand responses for dealer up card versus player pair
FOR x = 0 TO 9
    FOR y = 0 TO 9
        ERASE pfr: ERASE psr: ERASE psum: ERASE i: z = 0
        FOR pc1 = 0 TO 9
            FOR pc2 = pc1 TO 9
                IF y = pc1 AND pc1 = pc2 THEN
                    pn% = pc1 / 2 * (19 - pc1) + pc2
                    dn% = x
                    z = z + 1
                    psum(0, z) = aq(pn%, dn%, hr(pn%, dn%)) * prob(pn%, dn%)
                    psum(1, z) = aq(pn%, dn%, 30) * prob(pn%, dn%)
                    psum(2, z) = aq(pn%, dn%, 31) * prob(pn%, dn%)
                    psum(3, z) = aq(pn%, dn%, 32) * prob(pn%, dn%)
                    pfr(z) = hr(pn%, dn%)
                    IF pc1 >= pc2 THEN psr(z) = far(pn%, dn%)
                    IF pc1 < pc2 THEN psr(z) = sar(pn%, dn%)
                END IF
        NEXT pc2: NEXT pc1
        FOR k = 1 TO z
            psum(0, 0) = psum(0, 0) + psum(0, k): psum(1, 0) = psum(1, 0) + psum(1, k): psum(2, 0) = psum(2, 0) + psum(2, k): psum(3, 0) = psum(3, 0) + psum(3, k)
        NEXT k
        m = 0: q = psum(0, 0)
        FOR l = 0 TO 3
            IF psum(l, 0) > q THEN q = psum(l, 0): m = l
        NEXT l
        ERASE i
        FOR k = 1 TO z
        i(pfr(k)) = i(pfr(k)) + 1: NEXT k
        FOR l = 29 TO 0 STEP -1
            IF i(l) > i(34) THEN i(34) = i(l): IF m = 0 THEN i(33) = l
        NEXT l
        IF m = 1 THEN i(33) = 30
        IF m = 2 THEN i(33) = 31
        IF m = 3 THEN i(33) = 32
        IF g(i(33)) < 30 THEN
            IF g(i(33)) <= 2 * (y + 1) THEN hp(x, y) = 1: REM 1 is code for S - Stand
            IF g(i(33)) > 2 * (y + 1) THEN hp(x, y) = 2: REM 2 is code for H - Hit
        END IF
        IF g(i(33)) = 32 THEN
            ERASE i
            FOR k = 1 TO z
            i(psr(k)) = i(psr(k)) + 1: NEXT k
            FOR l = 32 TO 0 STEP -1
                IF i(l) > i(34) THEN i(34) = i(l): i(33) = l
            NEXT l
            IF g(i(33)) <= 2 * (y + 1) THEN hp(x, y) = 3: REM 3 is code for Ds - Double else Stand.
            IF g(i(33)) > 2 * (y + 1) THEN hp(x, y) = 4: REM 4 is code for Dh - Double else Hit.
            IF g(i(33)) = 32 THEN hp(x, y) = 5: REM 5 is code for D - Double down.
        END IF
        IF g(i(33)) = 31 THEN
            ERASE i
            FOR k = 1 TO z
            i(psr(k)) = i(psr(k)) + 1: NEXT k
            FOR l = 32 TO 0 STEP -1
                IF i(l) > i(34) THEN i(34) = i(l): i(33) = l
            NEXT l
            IF g(i(33)) <= 2 * (y + 1) THEN hp(x, y) = 9: REM 9 is code for R* - Surrender else Stand.
            IF g(i(33)) > 2 * (y + 1) THEN hp(x, y) = 10: REM 10 is code for R - Surrender else Hit.
        END IF
        IF g(i(33)) = 30 THEN
            ERASE i
            FOR k = 1 TO z
            i(psr(k)) = i(psr(k)) + 1: NEXT k
            FOR l = 32 TO 0 STEP -1
                IF i(l) > i(34) THEN i(34) = i(l): i(33) = l
            NEXT l
            IF g(i(33)) <= 2 * (y + 1) THEN hp(x, y) = 6: REM 6 is code for PS - sPlit pairs until no longer permitted and then Stand.
            IF g(i(33)) > 2 * (y + 1) THEN hp(x, y) = 7: REM 7 is code for PH - sPlit pairs until no longer permitted and then Hit.
            IF g(i(33)) = 32 THEN hp(x, y) = 8: REM 8 is code for PD - sPlit pairs until no longer permitted and then Double down.
            IF y = 0 AND dsa% <> 1 THEN hp(x, y) = 6
        END IF
NEXT y: NEXT x
300 REM hh(9, 15) stores hard hand responses for dealer up card versus player hard hand
FOR x = 0 TO 9
    FOR y = 0 TO 15
        ERASE pfr: ERASE psr: ERASE psum: ERASE i: z = 0
        FOR pc1 = 0 TO 12
            IF pc1 > 9 THEN c1% = 9 ELSE c1% = pc1
            FOR pc2 = pc1 TO 12
                IF pc2 > 9 THEN c2% = 9 ELSE c2% = pc2
                IF c1% * c2% <> 0 AND c1% <> c2% THEN
                    IF y = c1% + c2% - 3 THEN
                        pn% = c1% / 2 * (19 - c1%) + c2%
                        dn% = x
                        z = z + 1
                        psum(0, z) = aq(pn%, dn%, hr(pn%, dn%)) * prob(pn%, dn%)
                        psum(2, z) = aq(pn%, dn%, 31) * prob(pn%, dn%)
                        psum(3, z) = aq(pn%, dn%, 32) * prob(pn%, dn%)
                        IF c1% >= c2% THEN psr(z) = far(pn%, dn%)
                        IF c1% < c2% THEN psr(z) = sar(pn%, dn%)
                    END IF
                END IF
        NEXT pc2: NEXT pc1
        FOR k = 1 TO z
            psum(0, 0) = psum(0, 0) + psum(0, k): psum(2, 0) = psum(2, 0) + psum(2, k): psum(3, 0) = psum(3, 0) + psum(3, k)
        NEXT k
        m = 0: q = psum(0, 0)
        FOR l = 0 TO 3
            IF l = 1 THEN l = 2
            IF psum(l, 0) > q THEN q = psum(l, 0): m = l
        NEXT l
        IF m = 0 THEN i(33) = rc(dn%)
        IF m = 2 THEN i(33) = 31
        IF m = 3 THEN i(33) = 32
        IF g(i(33)) < 30 THEN
            IF g(i(33)) <= y + 5 THEN hh(x, y) = 1: REM 1 is code for S - Stand
            IF g(i(33)) > y + 5 THEN
                IF g(rc(x)) > y + 5 THEN hh(x, y) = 2 ELSE hh(x, y) = 1
                REM 2 is code for H - Hit
            END IF
        END IF
        IF g(i(33)) = 32 THEN
            ERASE i
            FOR k = 1 TO z
            i(psr(k)) = i(psr(k)) + 1: NEXT k
            FOR l = 32 TO 0 STEP -1
                IF i(l) > i(34) THEN i(34) = i(l): i(33) = l
            NEXT l
            IF g(i(33)) <= y + 5 THEN hh(x, y) = 3: REM 3 is code for Ds - Double else Stand.
            IF g(i(33)) > y + 5 THEN hh(x, y) = 4: REM 4 is code for Dh - Double else Hit.
            IF g(i(33)) = 32 THEN hh(x, y) = 5: REM 5 is code for D - Double down.
        END IF
        IF g(i(33)) = 31 THEN
            ERASE i
            FOR k = 1 TO z
            i(psr(k)) = i(psr(k)) + 1: NEXT k
            FOR l = 32 TO 0 STEP -1
                IF i(l) > i(34) THEN i(34) = i(l): i(33) = l
            NEXT l
            IF g(i(33)) <= y + 5 THEN hh(x, y) = 9: REM 9 is code for R* - Surrender else Stand.
            IF g(i(33)) > y + 5 THEN hh(x, y) = 10: REM 10 is code for R - Surrender else Hit.
        END IF
NEXT y: NEXT x
400 REM ss(9, 7) stores soft hand responses for dealer up card versus player soft hand
FOR x = 0 TO 9
    FOR y = 0 TO 7
        ERASE pfr: ERASE psr: ERASE psum: ERASE i: z = 0
        FOR pc1 = 0 TO 12
            IF pc1 > 9 THEN c1% = 9 ELSE c1% = pc1
            FOR pc2 = pc1 TO 12
                IF pc2 > 9 THEN c2% = 9 ELSE c2% = pc2
                IF c1% * c2% = 0 AND c1% <> c2% THEN
                    IF y = c1% + c2% - 1 THEN
                        pn% = c1% / 2 * (19 - c1%) + c2%
                        dn% = x
                        z = z + 1
                        psum(0, z) = aq(pn%, dn%, hr(pn%, dn%)) * prob(pn%, dn%)
                        psum(2, z) = aq(pn%, dn%, 31) * prob(pn%, dn%)
                        psum(3, z) = aq(pn%, dn%, 32) * prob(pn%, dn%)
                        IF dsa% = 1 OR c1% <> 0 THEN
                            IF c1% > c2% THEN psr(z) = far(pn%, dn%)
                            IF c1% <= c2% THEN psr(z) = sar(pn%, dn%)
                        END IF
                        IF dsa% <> 1 AND c1% = 0 THEN
                            psr(z) = far(pn%, dn%)
                        END IF
                    END IF
                END IF
        NEXT pc2: NEXT pc1
        FOR k = 1 TO z
            psum(0, 0) = psum(0, 0) + psum(0, k): psum(2, 0) = psum(2, 0) + psum(2, k): psum(3, 0) = psum(3, 0) + psum(3, k)
        NEXT k
        m = 0: q = psum(0, 0)
        FOR l = 0 TO 3
            IF l = 1 THEN l = 2
            IF psum(l, 0) > q THEN q = psum(l, 0): m = l
        NEXT l
        IF m = 0 THEN i(33) = rc(dn%)
        IF m = 2 THEN i(33) = 31
        IF m = 3 THEN i(33) = 32
        IF h(i(33)) < 30 THEN
            IF h(i(33)) <= y + 13 THEN ss(x, y) = 1: REM 1 is code for S - Stand
            IF h(i(33)) > y + 13 THEN ss(x, y) = 2: REM 2 is code for H - Hit
        END IF
        IF h(i(33)) = 32 THEN
            ERASE i
            FOR k = 1 TO z
            i(psr(k)) = i(psr(k)) + 1: NEXT k
            FOR l = 32 TO 0 STEP -1
                IF i(l) > i(34) THEN i(34) = i(l): i(33) = l
            NEXT l
            IF h(i(33)) <= y + 13 THEN ss(x, y) = 3: REM 3 is code for Ds - Double else Stand.
            IF h(i(33)) > y + 13 THEN ss(x, y) = 4: REM 4 is code for Dh - Double else Hit.
            IF h(i(33)) = 32 THEN ss(x, y) = 5: REM 5 is code for D - Double down.
        END IF
        IF h(i(33)) = 31 THEN
            ERASE i
            FOR k = 1 TO z
            i(psr(k)) = i(psr(k)) + 1: NEXT k
            FOR l = 32 TO 0 STEP -1
                IF i(l) > i(34) THEN i(34) = i(l): i(33) = l
            NEXT l
            IF h(i(33)) <= y + 13 THEN ss(x, y) = 9: REM 9 is code for R* - Surrender else Stand.
            IF h(i(33)) > y + 13 THEN ss(x, y) = 10: REM 10 is code for R - Surrender else Hit.
        END IF
NEXT y: NEXT x
500 REM m(9, 33) stores code for the entire simplified basic playing response strategy
FOR x = 0 TO 9
    FOR y = 0 TO 33
        IF y < 16 THEN
            IF x = 0 THEN m(x + 9, y) = hh(x, y)
            IF x > 0 THEN m(x - 1, y) = hh(x, y)
        END IF
        IF y > 15 AND y < 24 THEN
            IF x = 0 THEN m(x + 9, y) = ss(x, 23 - y)
            IF x > 0 THEN m(x - 1, y) = ss(x, 23 - y)
        END IF
        IF y > 23 THEN
            IF x = 0 THEN m(x + 9, y) = hp(x, y - 24)
            IF x > 0 THEN m(x - 1, y) = hp(x, y - 24)
        END IF
NEXT y: NEXT x
700 REM print basic strategy for two card hands to a text file.
v$ = files$ + "c.txt"
OPEN v$ FOR OUTPUT AS #1
PRINT #1, "the most favorable player responses": PRINT #1, " "
dh$ = "2 3 4 5 6 7 8 9 T A "
ph$ = "05060708091011121314151617181920A9A8A7A6A5A4A3A2AA2233445566778899TT"
q$ = "S H DsDhD PSPHPDR*R "
FOR x = 0 TO 10
    IF x = 0 THEN PRINT #1, "     ";
    IF x > 0 THEN PRINT #1, MID$(dh$, x * 2 - 1, 2);: PRINT #1, " ";
NEXT x
PRINT #1, " "
FOR x = 0 TO 10
    IF x = 0 THEN PRINT #1, "   ";
NEXT x
PRINT #1, "    "
FOR y = 0 TO 33
    FOR x = 0 TO 11
        IF x = 0 THEN PRINT #1, MID$(ph$, y * 2 + 1, 2); "   ";
        IF x > 0 AND x < 11 THEN PRINT #1, MID$(q$, (m(x - 1, y) - 1) * 2 + 1, 2); " ";
        IF x = 11 THEN PRINT #1, " "; MID$(ph$, y * 2 + 1, 2);
NEXT x: PRINT #1, " ": NEXT y
PRINT #1, " "
FOR x = 0 TO 10
    IF x = 0 THEN PRINT #1, "     ";
    IF x > 0 THEN PRINT #1, MID$(dh$, x * 2 - 1, 2);: PRINT #1, " ";
NEXT x
PRINT #1, " "
PRINT #1, " "
PRINT #1, t$
PRINT #1, " "
PRINT #1, "Key: S = Stand, H = Hit"
PRINT #1, "     D = Double, Ds = Double else Stand, Dh = Double else Hit"
PRINT #1, "     PS = Split else Stand, PH = Split else Hit, PD = Split else Double, "
PRINT #1, "     R* = Surrender else Stand, R = Surrender else Hit"
PRINT #1, " "
PRINT #1, "Breakdown ": PRINT #1, " "
PRINT #1, "Stand       ";: PRINT #1, USING "###.###"; pop(1) * 100;: PRINT #1, " % of the time."
PRINT #1, "Hit         ";: PRINT #1, USING "###.###"; pop(2) * 100;: PRINT #1, " % of the time."
PRINT #1, "Double Down ";: PRINT #1, USING "###.###"; pop(3) * 100;: PRINT #1, " % of the time."
PRINT #1, "Split       ";: PRINT #1, USING "###.###"; pop(4) * 100;: PRINT #1, " % of the time."
PRINT #1, "Surrender   ";: PRINT #1, USING "###.###"; pop(5) * 100;: PRINT #1, " % of the time."
CLOSE #1
ERASE br: ERASE far: ERASE sar
800 REM put a human friendly basic strategy into a computer friendly advanced combination dependent strategy format
FOR pc1 = 0 TO 9
    FOR pc2 = pc1 TO 9
        pn% = pc1 / 2 * (19 - pc1) + pc2
        FOR dc1 = 0 TO 9
            dn% = dc1
            IF pc1 * pc2 <> 0 AND pc1 <> pc2 THEN
                pe% = (pc1 + pc2 + 2) - 5
                de% = dc1
                IF hh(de%, pe%) = 1 THEN br(pn%, dn%) = 1: far(pn%, dn%) = 1: sar(pn%, dn%) = 1
                IF hh(de%, pe%) = 2 THEN br(pn%, dn%) = rc(de%): far(pn%, dn%) = rc(de%): sar(pn%, dn%) = rc(de%)
                IF hh(de%, pe%) = 3 THEN br(pn%, dn%) = 32: far(pn%, dn%) = 1: sar(pn%, dn%) = 1
                IF hh(de%, pe%) = 4 THEN br(pn%, dn%) = 32: far(pn%, dn%) = rc(de%): sar(pn%, dn%) = rc(de%)
                IF hh(de%, pe%) = 5 THEN br(pn%, dn%) = 32: far(pn%, dn%) = 32: sar(pn%, dn%) = 32
                IF hh(de%, pe%) = 6 THEN br(pn%, dn%) = 30: far(pn%, dn%) = 1: sar(pn%, dn%) = 1
                IF hh(de%, pe%) = 7 THEN br(pn%, dn%) = 30: far(pn%, dn%) = rc(de%): sar(pn%, dn%) = rc(de%)
                IF hh(de%, pe%) = 8 THEN br(pn%, dn%) = 30: far(pn%, dn%) = 32: sar(pn%, dn%) = 32
                IF hh(de%, pe%) = 9 THEN br(pn%, dn%) = 31: far(pn%, dn%) = 1: sar(pn%, dn%) = 1
                IF hh(de%, pe%) = 10 THEN br(pn%, dn%) = 31: far(pn%, dn%) = rc(de%): sar(pn%, dn%) = rc(de%)
            END IF
            IF pc1 * pc2 = 0 AND pc1 <> pc2 THEN
                pe% = (pc1 + pc2 + 2) - 3
                de% = dc1
                IF pe% = 8 THEN br(pn%, dn%) = 1: far(pn%, dn%) = 1: sar(pn%, dn%) = 1
                IF pe% < 8 THEN
                    IF ss(de%, pe%) = 1 THEN br(pn%, dn%) = 1: far(pn%, dn%) = 1: sar(pn%, dn%) = 1
                    IF ss(de%, pe%) = 2 THEN br(pn%, dn%) = rc(de%): far(pn%, dn%) = rc(de%): sar(pn%, dn%) = rc(de%)
                    IF ss(de%, pe%) = 3 THEN br(pn%, dn%) = 32: far(pn%, dn%) = 1: sar(pn%, dn%) = 1
                    IF ss(de%, pe%) = 4 THEN br(pn%, dn%) = 32: far(pn%, dn%) = rc(de%): sar(pn%, dn%) = rc(de%)
                    IF ss(de%, pe%) = 5 THEN br(pn%, dn%) = 32: far(pn%, dn%) = 32: sar(pn%, dn%) = 32
                    IF ss(de%, pe%) = 6 THEN br(pn%, dn%) = 30: far(pn%, dn%) = 1: sar(pn%, dn%) = 1
                    IF ss(de%, pe%) = 7 THEN br(pn%, dn%) = 30: far(pn%, dn%) = rc(de%): sar(pn%, dn%) = rc(de%)
                    IF ss(de%, pe%) = 8 THEN br(pn%, dn%) = 30: far(pn%, dn%) = 32: sar(pn%, dn%) = 32
                    IF ss(de%, pe%) = 9 THEN br(pn%, dn%) = 31: far(pn%, dn%) = 1: sar(pn%, dn%) = 1
                    IF ss(de%, pe%) = 10 THEN br(pn%, dn%) = 31: far(pn%, dn%) = rc(de%): sar(pn%, dn%) = rc(de%)
                END IF
            END IF
            IF pc1 = pc2 THEN
                pe% = pc1
                de% = dc1
                IF hp(de%, pe%) = 1 THEN br(pn%, dn%) = 1: far(pn%, dn%) = 1: sar(pn%, dn%) = 1
                IF hp(de%, pe%) = 2 THEN br(pn%, dn%) = rc(de%): far(pn%, dn%) = rc(de%): sar(pn%, dn%) = rc(de%)
                IF hp(de%, pe%) = 3 THEN br(pn%, dn%) = 32: far(pn%, dn%) = 1: sar(pn%, dn%) = 1
                IF hp(de%, pe%) = 4 THEN br(pn%, dn%) = 32: far(pn%, dn%) = rc(de%): sar(pn%, dn%) = rc(de%)
                IF hp(de%, pe%) = 5 THEN br(pn%, dn%) = 32: far(pn%, dn%) = 32: sar(pn%, dn%) = 32
                IF hp(de%, pe%) = 6 THEN br(pn%, dn%) = 30: far(pn%, dn%) = 1: sar(pn%, dn%) = 1
                IF hp(de%, pe%) = 7 THEN br(pn%, dn%) = 30: far(pn%, dn%) = rc(de%): sar(pn%, dn%) = rc(de%)
                IF hp(de%, pe%) = 8 THEN br(pn%, dn%) = 30: far(pn%, dn%) = 32: sar(pn%, dn%) = 32
                IF hp(de%, pe%) = 9 THEN br(pn%, dn%) = 31: far(pn%, dn%) = 1: sar(pn%, dn%) = 1
                IF hp(de%, pe%) = 10 THEN br(pn%, dn%) = 31: far(pn%, dn%) = rc(de%): sar(pn%, dn%) = rc(de%)
            END IF
NEXT dc1: NEXT pc2: NEXT pc1
v$ = files$ + "a.dat"
OPEN v$ FOR RANDOM AS #1 LEN = LEN(sd)
sd.c1 = bc(0): sd.c2 = bc(1): sd.c3 = bc(2): sd.c4 = bc(3): sd.c5 = bc(4)
sd.c6 = bc(5): sd.c7 = bc(6): sd.c8 = bc(7): sd.c9 = bc(8): sd.cT = bc(9)
sd.cJ = bc(10): sd.cQ = bc(11): sd.cK = bc(12): sd.dwt = dwt%: sd.dbc = dbc%
sd.ls = ls%: sd.early = early%: sd.j = j: sd.hole = hole%: sd.push = push%: sd.ten = ten%
sd.schw = schw%: sd.splt = splt%: sd.nsh = most%: sd.rsa = rsa%: sd.das = das%
sd.dh = dh%: sd.ld = ld%: sd.blackjack = blackjack: sd.ins = ins%: sd.dsa = dsa%
sd.bjsh = bjsh%: sd.deck = deck%: sd.shf = shf: sd.immed = immed%: sd.pbd = pbd%
sd.b21po = b21po%: sd.ms777 = ms777: sd.ss777 = ss777: sd.sp777 = sp777: sd.jp777 = jp777
sd.po5c21 = mc21po(4): sd.po6c21 = mc21po(5): sd.po7c21 = mc21po(6): sd.po8c21 = mc21po(7)
sd.ms678 = ms678: sd.ss678 = ss678: sd.sp678 = sp678
IF LEN(q$) > 80 THEN sd.description = MID$(q$, 1, 80) ELSE sd.description = files$
PUT #1, , sd
CLOSE #1
v$ = files$ + "b.dat"
w = 0
OPEN v$ FOR RANDOM AS #1 LEN = LEN(num)
FOR x = 0 TO 54
    FOR y = 0 TO 9
        w = w + 1
        num.far = far(x, y): num.sar = sar(x, y): num.r = br(x, y): num.hit = hit(x, y)
        PUT #1, w, num
NEXT y: NEXT x
CLOSE #1
z = 0
FOR x = 12 TO 19
    FOR y = 5 TO 8
        IF x > y + 12 THEN y = x - 12
        z = z + 1
        g(z) = x: h(z) = y
NEXT y: NEXT x: g(30) = 1000: g(31) = 1000: h(30) = 1000: h(31) = 1000: h(32) = 1000: g(32) = 1000
v$ = files$ + "o.txt"
OPEN v$ FOR OUTPUT AS #1
PRINT #1, "the most favorable player responses": PRINT #1, t$
b$ = "AAA2A3A4A5A6A7A8A9AJ22232425262728292J333435363738393J4445464748494J55565758595J666768696J7778797J88898J999JJJ"
FOR x = 0 TO 10
    IF x = 0 THEN PRINT #1, "     ";
    IF x > 0 THEN PRINT #1, USING "##"; x;: PRINT #1, "     ";
NEXT x
PRINT #1, " "
FOR x = 0 TO 10
    IF x = 0 THEN PRINT #1, "   ";
NEXT x
PRINT #1, "    "
FOR w = 0 TO 9
    FOR x = w TO 9
        n = w / 2 * (19 - w) + x
        FOR y = 0 TO 10
            IF y = 0 THEN PRINT #1, MID$(b$, n * 2 + 1, 2); "  ";
            IF y <> 0 THEN
                IF w + x < h(br(n, y - 1)) THEN
                    IF w * x <> 0 THEN
                        IF w + x + 2 >= g(br(n, y - 1)) THEN PRINT #1, "Stand  ";
                        IF w + x + 2 < g(br(n, y - 1)) THEN PRINT #1, MID$(o$, br(n, y - 1) * 5 + 1, 5); "  ";
                    END IF
                    IF w * x = 0 THEN
                        IF w + x >= h(br(n, y - 1)) THEN PRINT #1, "Stand  ";
                        IF w + x < h(br(n, y - 1)) THEN PRINT #1, MID$(o$, br(n, y - 1) * 5 + 1, 5); "  ";
                    END IF
                END IF
                IF w + x >= h(br(n, y - 1)) THEN
                    IF w * x <> 0 THEN
                        IF w + x + 2 >= g(br(n, y - 1)) THEN PRINT #1, "Stand  ";
                        IF w + x + 2 < g(br(n, y - 1)) THEN PRINT #1, MID$(n$, br(n, y - 1) * 5 + 1, 5); "  ";
                    END IF
                    IF w * x = 0 THEN
                        IF w + x >= h(br(n, y - 1)) THEN PRINT #1, "Stand  ";
                        IF w + x < h(br(n, y - 1)) THEN PRINT #1, MID$(n$, br(n, y - 1) * 5 + 1, 5); "  ";
                    END IF
                END IF
            END IF
NEXT y: PRINT #1, " ": NEXT x: NEXT w
PRINT #1, " "
CLS: PRINT #1, "the most favorable player responses after split with pc1>=pc2": PRINT #1, " "
b$ = "AA2A3A4A5A6A7A8A9AJA2232425262728292J233435363738393J3445464748494J45565758595J566768696J6778797J78898J899J9JJ"
FOR x = 0 TO 10
    IF x = 0 THEN PRINT #1, "     ";
    IF x > 0 THEN PRINT #1, USING "##"; x;: PRINT #1, "     ";
NEXT x
PRINT #1, " "
FOR x = 0 TO 10
    IF x = 0 THEN PRINT #1, "   ";
NEXT x
PRINT #1, "    "
FOR w = 0 TO 9
    FOR x = w TO 9
        n = w / 2 * (19 - w) + x
        FOR y = 0 TO 10
            IF y = 0 THEN PRINT #1, MID$(b$, n * 2 + 1, 2); "  ";
            IF y <> 0 THEN
                IF w + x < h(far(n, y - 1)) THEN
                    IF w * x <> 0 THEN
                        IF w + x + 2 >= g(far(n, y - 1)) THEN PRINT #1, "Stand";
                        IF w + x + 2 < g(far(n, y - 1)) THEN PRINT #1, MID$(o$, far(n, y - 1) * 5 + 1, 5);
                    END IF
                    IF w * x = 0 THEN
                        IF w + x = 0 AND dsa% <> 1 THEN PRINT #1, "Stand";
                        IF w + x <> 0 OR dsa% = 1 THEN PRINT #1, MID$(o$, far(n, y - 1) * 5 + 1, 5);
                    END IF
                    IF far(n, y - 1) <> br(n, y - 1) THEN PRINT #1, "* ";
                    IF far(n, y - 1) = br(n, y - 1) THEN PRINT #1, "  ";
                END IF
                IF w + x >= h(far(n, y - 1)) THEN
                    IF w * x <> 0 THEN
                        IF w + x + 2 >= g(far(n, y - 1)) THEN PRINT #1, "Stand";
                        IF w + x + 2 < g(far(n, y - 1)) THEN PRINT #1, MID$(n$, far(n, y - 1) * 5 + 1, 5);
                    END IF
                    IF w * x = 0 THEN PRINT #1, "Stand";
                    IF far(n, y - 1) <> br(n, y - 1) THEN PRINT #1, "* ";
                    IF far(n, y - 1) = br(n, y - 1) THEN PRINT #1, "  ";
                END IF
            END IF
NEXT y: PRINT #1, " ": NEXT x: NEXT w
PRINT #1, " "
CLS: PRINT #1, "the most favorable player responses after split with pc1<=pc2": PRINT #1, " "
b$ = "AAA2A3A4A5A6A7A8A9AJ22232425262728292J333435363738393J4445464748494J55565758595J666768696J7778797J88898J999JJJ"
FOR x = 0 TO 10
    IF x = 0 THEN PRINT #1, "     ";
    IF x > 0 THEN PRINT #1, USING "##"; x;: PRINT #1, "     ";
NEXT x
PRINT #1, " "
FOR x = 0 TO 10
    IF x = 0 THEN PRINT #1, "   ";
NEXT x
PRINT #1, "    "
FOR w = 0 TO 9
    FOR x = w TO 9
        n = w / 2 * (19 - w) + x
        FOR y = 0 TO 10
            IF y = 0 THEN PRINT #1, MID$(b$, n * 2 + 1, 2); "  ";
            IF y <> 0 THEN
                IF w + x < h(sar(n, y - 1)) THEN
                    IF w * x <> 0 THEN
                        IF w + x + 2 >= g(sar(n, y - 1)) THEN PRINT #1, "Stand";
                        IF w + x + 2 < g(sar(n, y - 1)) THEN PRINT #1, MID$(o$, sar(n, y - 1) * 5 + 1, 5);
                    END IF
                    IF w * x = 0 THEN
                        IF w = 0 AND dsa% <> 1 THEN PRINT #1, "Stand";
                        IF w <> 0 OR dsa% = 1 THEN PRINT #1, MID$(o$, sar(n, y - 1) * 5 + 1, 5);
                    END IF
                    IF sar(n, y - 1) <> br(n, y - 1) THEN PRINT #1, "* ";
                    IF sar(n, y - 1) = br(n, y - 1) THEN PRINT #1, "  ";
                END IF
                IF w + x >= h(sar(n, y - 1)) THEN
                    IF w * x <> 0 THEN
                        IF w + x + 2 >= g(sar(n, y - 1)) THEN PRINT #1, "Stand";
                        IF w + x + 2 < g(sar(n, y - 1)) THEN PRINT #1, MID$(n$, sar(n, y - 1) * 5 + 1, 5);
                    END IF
                    IF w * x = 0 THEN PRINT #1, "Stand";
                    IF sar(n, y - 1) <> br(n, y - 1) THEN PRINT #1, "* ";
                    IF sar(n, y - 1) = br(n, y - 1) THEN PRINT #1, "  ";
                END IF
            END IF
NEXT y: PRINT #1, " ": NEXT x: NEXT w
PRINT #1, " "
CLOSE #1
END
