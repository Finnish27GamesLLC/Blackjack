_TITLE "ULTIMA221.BAS"
TYPE comp
    adc0 AS DOUBLE
    adc1 AS DOUBLE
    adc2 AS DOUBLE
    adc3 AS DOUBLE
    adc4 AS DOUBLE
    adc5 AS DOUBLE
    adc6 AS DOUBLE
    adc7 AS DOUBLE
    adc8 AS DOUBLE
    adc9 AS DOUBLE
    adc10 AS DOUBLE
    adc11 AS DOUBLE
    adc12 AS DOUBLE
    adc13 AS DOUBLE
    adc14 AS DOUBLE
    hmr AS LONG
    ap AS INTEGER
    description AS STRING * 80
END TYPE
TYPE short
    nor AS INTEGER
    fr AS INTEGER
    fl AS SINGLE
    sr AS INTEGER
    sl AS SINGLE
    tr AS INTEGER
    tl AS SINGLE
    rr AS INTEGER
    rl AS SINGLE
    vr AS INTEGER
    vl AS SINGLE
    xr AS INTEGER
    xl AS SINGLE
    nr AS INTEGER
    nl AS SINGLE
    er AS INTEGER
    el AS SINGLE
    nnr AS INTEGER
    nnl AS SINGLE
    lr AS INTEGER
    ll AS SINGLE
END TYPE
TYPE str
    description AS STRING * 80
END TYPE
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
DIM database AS xbase: DIM sd AS bsrules: DIM num AS bs: DIM wow AS str: DIM mdc AS comp: DIM adc(14, 600) AS DOUBLE: DIM dd(3 TO 7) AS INTEGER
DIM ir(54, 9) AS short: DIM ifar(54, 9) AS short: DIM isar(54, 9) AS short: DIM g(32) AS INTEGER: DIM h(32) AS INTEGER: DIM mc(22): DIM card(22)
DIM hit(54, 9): DIM sar(54, 9) AS INTEGER: DIM far(54, 9) AS INTEGER: DIM br(54, 9) AS INTEGER: DIM aq(54, 9, 32): DIM psum(3, 36)
DIM ss(9, 7) AS INTEGER: DIM hh(9, 15) AS INTEGER: DIM hp(9, 9) AS INTEGER: DIM prob(54, 9): DIM htotal(9, 32): DIM stotal(9, 32)
DIM mc21po(11) AS SINGLE: DIM m(600, 9, 33) AS INTEGER: DIM pfr(36) AS INTEGER: DIM psr(36) AS INTEGER: DIM i(34) AS INTEGER: DIM hr(54, 9) AS INTEGER
DIM hf(9) AS INTEGER: DIM sf(9) AS INTEGER: DIM arc(9, 9) AS INTEGER: DIM rc(9) AS INTEGER: DIM sprob(9, 32): DIM hprob(9, 32): DIM pop(5) AS SINGLE: DIM im(9, 33) AS short
DIM mp(9, 33) AS INTEGER: DIM mv(9, 33) AS INTEGER: DIM bc(12) AS INTEGER
CLS: PRINT "ULTIMA299.BAS loads data saved by ULTIMA298.BAS about average stack compositions"
PRINT "that occur at various card count totals during a simulated game of blackjack"
PRINT "and generates indices. ULTIMA221.BAS loads data from these files"
PRINT "and derives a total dependent basic playing strategy from each"
PRINT "combination dependent advanced playing strategy saved by ULTIMA299.BAS"
PRINT "for each count level average stack composition saved by ULTIMA298.BAS"
PRINT "ULTIMA221.BAS organizes these total dependent basic playing strategies into"
PRINT "tables and saves these tables to text files. ULTIMA221.BAS uses these"
PRINT "total dependent basic playing strategies to reassemble count level indices"
PRINT "and saves these count level indices in data files that may loaded into"
PRINT "game simulator ULTIMA297.BAS to test for improvements in playing efficiency."
PRINT "ULTIMA221.BAS also saves these indices to text files. Users may study these"
PRINT "text files, learn these count level indices and know how to make"
PRINT "play variations in response to changes in the count level during a live game"
PRINT "of casino blackjack."
PRINT ""
69 PRINT "enter 'folder/filename prefix' to be used to store the input screen and program output": INPUT files$
PRINT "enter 'folder/filename prefix'of matching indices": INPUT indx$
OPEN indx$ + "d.dat" FOR RANDOM AS #1 LEN = LEN(wow)
GET #1, , wow
CLOSE #1: x$ = wow.description
PRINT "matching indices are described as follows:"; x$
PRINT "if this is acceptable type 'y'and press enter";: INPUT ans$: IF ans$ <> "y" THEN GOTO 69
OPEN files$ + "i.txt" FOR OUTPUT AS #1
PRINT #1, "enter 'folder/filename prefix' to be used to store the input screen and program output": PRINT #1, files$
PRINT #1, "enter 'folder/filename prefix'of matching indices": PRINT #1, indx$
PRINT #1, "matching indices are described as follows:": PRINT #1, x$
PRINT #1, "if this is acceptable type 'y'and press enter": PRINT #1, ans$
CLOSE #1
o$ = "     12/A612/A712/A812/A913/A613/A713/A813/A914/A614/A714/A814/A915/A615/A715/A815/A916/A616/A716/A816/A917/A617/A717/A817/A918/A718/A818/A919/A819/A9SplitSrndrDDown"
n$ = "     12   12   12   12   13   13   13   13   14   14   14   14   15   15   15   15   16   16   16   16   17   17   17   17   18   18   18   19   19   SplitSrndrDDown"
70 PRINT "enter 'folder/filename prefix' of average deck compositions ": INPUT a$
OPEN a$ + "adc.dat" FOR RANDOM AS #1 LEN = LEN(mdc)
GET #1, , mdc
dc$ = mdc.description
CLOSE #1
PRINT "deck compositions loaded are described as follows:"; dc$
PRINT "if this is acceptable type 'y'and press enter";: INPUT ans$
OPEN files$ + "i.txt" FOR APPEND AS #1
PRINT #1, "enter 'folder/filename prefix' of average deck compositions ";: PRINT #1, a$
PRINT #1, "deck compositions loaded are described as follows: ": PRINT #1, dc$
PRINT #1, "if this is acceptable type 'y'and press enter ": PRINT #1, ans$
CLOSE #1
IF ans$ <> "y" THEN GOTO 70
hmr& = mdc.hmr: ap% = mdc.ap
PRINT "input the minimum number of samples required to trigger this program"
PRINT "to produce an optimal playing strategy for an average deck composition"
PRINT "and use this in the process of creating playing indices.": INPUT sample&
PRINT "input a description of indices": INPUT ndx$
wow.description = ndx$
OPEN files$ + "i.txt" FOR APPEND AS #1
PRINT #1, "input the minimum number of samples required to trigger this program"
PRINT #1, "to produce an optimal playing strategy for an average deck composition"
PRINT #1, "and use this in the process of creating playing indices. ": PRINT #1, sample&
PRINT #1, "input a description of indices ";: PRINT #1, ndx$
CLOSE #1
819 PRINT "type 'y' and enter to save count level strategies": INPUT ans$
OPEN files$ + "i.txt" FOR APPEND AS #1
PRINT #1, "type 'y' and enter to save count level strategies ": PRINT #1, ans$
CLOSE #1
IF ans$ = "y" THEN
    PRINT "type 'y' and enter to save each strategy generated for each deck composition": INPUT ans$: IF ans$ = "y" THEN ss% = 1
    OPEN files$ + "i.txt" FOR APPEND AS #1
    PRINT #1, "type 'y' and enter to save each strategy generated for each deck composition ";: PRINT #1, ans$
    CLOSE #1
    PRINT "type 'y' and enter to save strategy tables generated for each deck composition": INPUT ans$: IF ans$ = "y" THEN sp% = 1
    OPEN files$ + "i.txt" FOR APPEND AS #1
    PRINT #1, "type 'y' and enter to save strategy tables generated for each deck composition ";: PRINT #1, ans$
    CLOSE #1
END IF
I = 0
OPEN a$ + "adc.dat" FOR RANDOM AS #1 LEN = LEN(mdc)
number = LOF(1) / LEN(mdc)
FOR x = 1 TO number
    GET #1, x, mdc
    adc(0, dcs) = mdc.adc0: adc(1, dcs) = mdc.adc1: adc(2, dcs) = mdc.adc2
    adc(3, dcs) = mdc.adc3: adc(4, dcs) = mdc.adc4: adc(5, dcs) = mdc.adc5
    adc(6, dcs) = mdc.adc6: adc(7, dcs) = mdc.adc7: adc(8, dcs) = mdc.adc8
    adc(9, dcs) = mdc.adc9: adc(10, dcs) = mdc.adc10: adc(11, dcs) = mdc.adc11
    adc(12, dcs) = mdc.adc12: adc(13, dcs) = mdc.adc13: adc(14, dcs) = mdc.adc14: dcs = dcs + 1
NEXT x: CLOSE #1
FOR level = 0 TO 600
    IF adc(14, level) < sample& THEN GOTO 949
    FOR x = 0 TO 13
        IF x < 10 THEN adc(x, level) = adc(x, level) / adc(14, level)
        IF x > 9 AND x < 13 THEN adc(9, level) = adc(9, level) + adc(x, level) / adc(14, level)
        IF x = 13 THEN adc(10, level) = adc(13, level) / adc(14, level)
    NEXT x
    twentysixes% = INT((level) / 26): ones% = INT(level - twentysixes% * 26)
    v$ = indx$ + CHR$(twentysixes% + 97) + CHR$(ones% + 97) + "a.dat"
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
    IF card(6) <> 0 AND b21po% = 1 THEN
        mc21po(1) = (card(6) * deck%) * (card(6) * deck% - 1) * (card(6) * deck% - 2): mc21po(2) = deck% * (deck% - 1) * (deck% - 2) * 4 / mc21po(1)
        mc21po(3) = deck% * (deck% - 1) * (deck% - 2) / mc21po(1): mc21po(1) = ms777
        mc21po(11) = (jp777 - ms777) * mc21po(2): mc21po(3) = (sp777 - ss777) * mc21po(3): mc21po(2) = (ss777 - ms777) * mc21po(2)
    END IF
    IF card(5) * card(6) * card(7) <> 0 AND b21po% = 1 THEN
        mc21po(8) = card(5) * card(6) * card(7) * deck% ^ 3: mc21po(9) = deck% ^ 3 * 4 / mc21po(8)
        mc21po(10) = deck% ^ 3 * 1 / mc21po(8): mc21po(8) = ms678
        mc21po(9) = (ss678 - ms678) * mc21po(9): mc21po(10) = (sp678 - ss678) * mc21po(10)
    END IF
    v$ = indx$ + CHR$(twentysixes% + 97) + CHR$(ones% + 97) + "b.dat"
    OPEN v$ FOR RANDOM AS #1 LEN = LEN(num)
    w = 0
    FOR x = 0 TO 54
        FOR y = 0 TO 9
            w = w + 1
            GET #1, w, num
            br(x, y) = num.r: far(x, y) = num.far: sar(x, y) = num.sar: hit(x, y) = num.hit
    NEXT y: NEXT x
    CLOSE #1
    v$ = indx$ + CHR$(twentysixes% + 97) + CHR$(ones% + 97) + "t.dat"
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
    GOSUB 93
    GOSUB 100
    GOSUB 200
    GOSUB 300
    GOSUB 400
    GOSUB 500
    GOSUB 700
    GOSUB 800
    IF ss% = 1 THEN GOSUB 850
    IF sp% = 1 THEN GOSUB 900
    GOSUB 999
949 NEXT level
GOTO 952
93 z = 0
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
            mc(x) = adc(x, level): NEXT x
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
ERASE htotal: ERASE stotal
RETURN
100 REM figure player's hard and soft stand at totals for each dealer up card
FOR dc1 = 0 TO 9
    FOR pc1 = 1 TO 9
        FOR pc2 = pc1 TO 9
            n = pc1 / 2 * (19 - pc1) + pc2
            IF br(n, dc1) < 30 AND pc1 <> pc2 THEN
                IF pc1 + pc2 + 2 < g(br(n, dc1)) THEN
                    FOR op = 1 TO 29
                        htotal(dc1, op) = htotal(dc1, op) + aq(n, dc1, op) * prob(n, dc1): hprob(dc1, op) = hprob(dc1, op) + prob(n, dc1)
                    NEXT op
                END IF
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
            IF hf(dc1) = g(z) AND sf(dc1) = h(z) THEN rc(dc1) = z: REM PRINT level - 300, g(z), h(z)
    NEXT y: NEXT x
NEXT dc1: REM SLEEP 1
RETURN
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
            IF i(l) > i(34) THEN i(34) = i(l): arc(x, y) = l: IF m = 0 THEN i(33) = l
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
RETURN
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
RETURN
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
RETURN
500 REM m(9, 33) stores code for the entire simplified basic playing response strategy
FOR x = 0 TO 9
    FOR y = 0 TO 33
        IF y < 16 THEN
            IF x = 0 THEN m(level, x + 9, y) = hh(x, y)
            IF x > 0 THEN m(level, x - 1, y) = hh(x, y)
        END IF
        IF y > 15 AND y < 24 THEN
            IF x = 0 THEN m(level, x + 9, y) = ss(x, 23 - y)
            IF x > 0 THEN m(level, x - 1, y) = ss(x, 23 - y)
        END IF
        IF y > 23 THEN
            IF x = 0 THEN m(level, x + 9, y) = hp(x, y - 24)
            IF x > 0 THEN m(level, x - 1, y) = hp(x, y - 24)
        END IF
NEXT y: NEXT x
RETURN
700 REM print basic strategy for two card hands to a text file.
v$ = files$ + CHR$(twentysixes% + 97) + CHR$(ones% + 97)
OPEN v$ + "c.txt" FOR OUTPUT AS #1
PRINT #1, "the most favorable player responses for level ";: PRINT #1, USING "####"; level - 300
PRINT #1, "based on ";: PRINT #1, USING "##########"; adc(14, level);: PRINT #1, " samples"
PRINT #1, ""
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
        IF x > 0 AND x < 11 THEN PRINT #1, MID$(q$, (m(level, x - 1, y) - 1) * 2 + 1, 2); " ";
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
ERASE br: ERASE far: ERASE sar: ERASE pop
RETURN
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
                IF hp(de%, pe%) = 2 THEN br(pn%, dn%) = arc(de%, pe%): far(pn%, dn%) = arc(de%, pe%): sar(pn%, dn%) = arc(de%, pe%)
                IF hp(de%, pe%) = 3 THEN br(pn%, dn%) = 32: far(pn%, dn%) = 1: sar(pn%, dn%) = 1
                IF hp(de%, pe%) = 4 THEN br(pn%, dn%) = 32: far(pn%, dn%) = arc(de%, pe%): sar(pn%, dn%) = arc(de%, pe%)
                IF hp(de%, pe%) = 5 THEN br(pn%, dn%) = 32: far(pn%, dn%) = 32: sar(pn%, dn%) = 32
                IF hp(de%, pe%) = 6 THEN br(pn%, dn%) = 30: far(pn%, dn%) = 1: sar(pn%, dn%) = 1
                IF hp(de%, pe%) = 7 THEN br(pn%, dn%) = 30: far(pn%, dn%) = arc(de%, pe%): sar(pn%, dn%) = arc(de%, pe%)
                IF hp(de%, pe%) = 8 THEN br(pn%, dn%) = 30: far(pn%, dn%) = 32: sar(pn%, dn%) = 32
                IF hp(de%, pe%) = 9 THEN br(pn%, dn%) = 31: far(pn%, dn%) = 1: sar(pn%, dn%) = 1
                IF hp(de%, pe%) = 10 THEN br(pn%, dn%) = 31: far(pn%, dn%) = arc(de%, pe%): sar(pn%, dn%) = arc(de%, pe%)
            END IF
NEXT dc1: NEXT pc2: NEXT pc1
RETURN
850 OPEN v$ + "a.dat" FOR RANDOM AS #1 LEN = LEN(sd)
sd.c1 = bc(0): sd.c2 = bc(1): sd.c3 = bc(2): sd.c4 = bc(3): sd.c5 = bc(4)
sd.c6 = bc(5): sd.c7 = bc(6): sd.c8 = bc(7): sd.c9 = bc(8): sd.cT = bc(9)
sd.cJ = bc(10): sd.cQ = bc(11): sd.cK = bc(12): sd.dwt = swt%: sd.dbc = dbc%
sd.ls = ls%: sd.early = early%: sd.j = j: sd.hole = hole%: sd.push = push%: sd.ten = ten%
sd.schw = schw%: sd.splt = splt%: sd.nsh = most%: sd.rsa = rsa%: sd.das = das%
sd.dh = dh%: sd.ld = ld%: sd.blackjack = blackjack: sd.ins = ins%: sd.dsa = dsa%
sd.bjsh = bjsh%: sd.deck = deck%: sd.shf = shf: sd.immed = immed%: sd.pbd = pbd%
sd.b21po = b21po%: sd.ms777 = ms777: sd.ss777 = ss777: sd.sp777 = sp777: sd.jp777 = jp777
sd.po5c21 = mc21po(4): sd.po6c21 = mc21po(5): sd.po7c21 = mc21po(6): sd.po8c21 = mc21po(7)
sd.ms678 = ms678: sd.ss678 = ss678: sd.sp678 = sp678
IF LEN(q$) > 80 THEN sd.description = MID$(q$, 1, 80) ELSE sd.description = t$
PUT #1, , sd
CLOSE #1
w = 0
OPEN v$ + "b.dat" FOR RANDOM AS #1 LEN = LEN(num)
FOR x = 0 TO 54
    FOR y = 0 TO 9
        w = w + 1
        num.far = far(x, y): num.sar = sar(x, y): num.r = br(x, y): num.hit = hit(x, y)
        PUT #1, w, num
NEXT y: NEXT x
CLOSE #1
RETURN
900 z = 0
FOR x = 12 TO 19
    FOR y = 5 TO 8
        IF x > y + 12 THEN y = x - 12
        z = z + 1
        g(z) = x: h(z) = y
NEXT y: NEXT x: g(30) = 1000: g(31) = 1000: h(30) = 1000: h(31) = 1000: h(32) = 1000: g(32) = 1000
OPEN v$ + "o.txt" FOR OUTPUT AS #1
PRINT #1, "the most favorable player responses for level ";: PRINT #1, USING "####"; level - 300
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
RETURN
999 FOR w = 0 TO 9
    FOR x = w TO 9
        n = w / 2 * (19 - w) + x
        FOR y = 0 TO 9
            IF br(n, y) > 0 THEN
                IF ir(n, y).nor = 9 THEN IF ir(n, y).nnr <> br(n, y) THEN ir(n, y).nor = 10: ir(n, y).nnl = level - 300: ir(n, y).lr = br(n, y)
                IF ir(n, y).nor = 8 THEN IF ir(n, y).er <> br(n, y) THEN ir(n, y).nor = 9: ir(n, y).el = level - 300: ir(n, y).nnr = br(n, y)
                IF ir(n, y).nor = 7 THEN IF ir(n, y).nr <> br(n, y) THEN ir(n, y).nor = 8: ir(n, y).nl = level - 300: ir(n, y).er = br(n, y)
                IF ir(n, y).nor = 6 THEN IF ir(n, y).xr <> br(n, y) THEN ir(n, y).nor = 7: ir(n, y).xl = level - 300: ir(n, y).nr = br(n, y)
                IF ir(n, y).nor = 5 THEN IF ir(n, y).vr <> br(n, y) THEN ir(n, y).nor = 6: ir(n, y).vl = level - 300: ir(n, y).xr = br(n, y)
                IF ir(n, y).nor = 4 THEN IF ir(n, y).rr <> br(n, y) THEN ir(n, y).nor = 5: ir(n, y).rl = level - 300: ir(n, y).vr = br(n, y)
                IF ir(n, y).nor = 3 THEN IF ir(n, y).tr <> br(n, y) THEN ir(n, y).nor = 4: ir(n, y).tl = level - 300: ir(n, y).rr = br(n, y)
                IF ir(n, y).nor = 2 THEN IF ir(n, y).sr <> br(n, y) THEN ir(n, y).nor = 3: ir(n, y).sl = level - 300: ir(n, y).tr = br(n, y)
                IF ir(n, y).nor = 1 THEN IF ir(n, y).fr <> br(n, y) THEN ir(n, y).nor = 2: ir(n, y).fl = level - 300: ir(n, y).sr = br(n, y)
                IF ir(n, y).nor = 0 THEN
                    ir(n, y).nor = 1: ir(n, y).fr = br(n, y): ir(n, y).fl = 1000: ir(n, y).sl = 1000: ir(n, y).tl = 1000: ir(n, y).rl = 1000
                    ir(n, y).vl = 1000: ir(n, y).xl = 1000: ir(n, y).nl = 1000: ir(n, y).el = 1000: ir(n, y).nnl = 1000: ir(n, y).ll = 1000
                END IF
            END IF
            IF far(n, y) > 0 THEN
                IF ifar(n, y).nor = 9 THEN IF ifar(n, y).nnr <> far(n, y) THEN ifar(n, y).nor = 10: ifar(n, y).nnl = level - 300: ifar(n, y).lr = far(n, y)
                IF ifar(n, y).nor = 8 THEN IF ifar(n, y).er <> far(n, y) THEN ifar(n, y).nor = 9: ifar(n, y).el = level - 300: ifar(n, y).nnr = far(n, y)
                IF ifar(n, y).nor = 7 THEN IF ifar(n, y).nr <> far(n, y) THEN ifar(n, y).nor = 8: ifar(n, y).nl = level - 300: ifar(n, y).er = far(n, y)
                IF ifar(n, y).nor = 6 THEN IF ifar(n, y).xr <> far(n, y) THEN ifar(n, y).nor = 7: ifar(n, y).xl = level - 300: ifar(n, y).nr = far(n, y)
                IF ifar(n, y).nor = 5 THEN IF ifar(n, y).vr <> far(n, y) THEN ifar(n, y).nor = 6: ifar(n, y).vl = level - 300: ifar(n, y).xr = far(n, y)
                IF ifar(n, y).nor = 4 THEN IF ifar(n, y).rr <> far(n, y) THEN ifar(n, y).nor = 5: ifar(n, y).rl = level - 300: ifar(n, y).vr = far(n, y)
                IF ifar(n, y).nor = 3 THEN IF ifar(n, y).tr <> far(n, y) THEN ifar(n, y).nor = 4: ifar(n, y).tl = level - 300: ifar(n, y).rr = far(n, y)
                IF ifar(n, y).nor = 2 THEN IF ifar(n, y).sr <> far(n, y) THEN ifar(n, y).nor = 3: ifar(n, y).sl = level - 300: ifar(n, y).tr = far(n, y)
                IF ifar(n, y).nor = 1 THEN IF ifar(n, y).fr <> far(n, y) THEN ifar(n, y).nor = 2: ifar(n, y).fl = level - 300: ifar(n, y).sr = far(n, y)
                IF ifar(n, y).nor = 0 THEN
                    ifar(n, y).nor = 1: ifar(n, y).fr = far(n, y): ifar(n, y).fl = 1000: ifar(n, y).sl = 1000: ifar(n, y).tl = 1000: ifar(n, y).rl = 1000
                    ifar(n, y).vl = 1000: ifar(n, y).xl = 1000: ifar(n, y).nl = 1000: ifar(n, y).el = 1000: ifar(n, y).nnl = 1000: ifar(n, y).ll = 1000
                END IF
            END IF
            IF sar(n, y) > 0 THEN
                IF isar(n, y).nor = 9 THEN IF isar(n, y).nnr <> sar(n, y) THEN isar(n, y).nor = 10: isar(n, y).nnl = level - 300: isar(n, y).lr = sar(n, y)
                IF isar(n, y).nor = 8 THEN IF isar(n, y).er <> sar(n, y) THEN isar(n, y).nor = 9: isar(n, y).el = level - 300: isar(n, y).nnr = sar(n, y)
                IF isar(n, y).nor = 7 THEN IF isar(n, y).nr <> sar(n, y) THEN isar(n, y).nor = 8: isar(n, y).nl = level - 300: isar(n, y).er = sar(n, y)
                IF isar(n, y).nor = 6 THEN IF isar(n, y).xr <> sar(n, y) THEN isar(n, y).nor = 7: isar(n, y).xl = level - 300: isar(n, y).nr = sar(n, y)
                IF isar(n, y).nor = 5 THEN IF isar(n, y).vr <> sar(n, y) THEN isar(n, y).nor = 6: isar(n, y).vl = level - 300: isar(n, y).xr = sar(n, y)
                IF isar(n, y).nor = 4 THEN IF isar(n, y).rr <> sar(n, y) THEN isar(n, y).nor = 5: isar(n, y).rl = level - 300: isar(n, y).vr = sar(n, y)
                IF isar(n, y).nor = 3 THEN IF isar(n, y).tr <> sar(n, y) THEN isar(n, y).nor = 4: isar(n, y).tl = level - 300: isar(n, y).rr = sar(n, y)
                IF isar(n, y).nor = 2 THEN IF isar(n, y).sr <> sar(n, y) THEN isar(n, y).nor = 3: isar(n, y).sl = level - 300: isar(n, y).tr = sar(n, y)
                IF isar(n, y).nor = 1 THEN IF isar(n, y).fr <> sar(n, y) THEN isar(n, y).nor = 2: isar(n, y).fl = level - 300: isar(n, y).sr = sar(n, y)
                IF isar(n, y).nor = 0 THEN
                    isar(n, y).nor = 1: isar(n, y).fr = sar(n, y): isar(n, y).fl = 1000: isar(n, y).sl = 1000: isar(n, y).tl = 1000: isar(n, y).rl = 1000
                    isar(n, y).vl = 1000: isar(n, y).xl = 1000: isar(n, y).nl = 1000: isar(n, y).el = 1000: isar(n, y).nnl = 1000: isar(n, y).ll = 1000
                END IF
            END IF
NEXT y: NEXT x: NEXT w
RETURN
952 OPEN files$ + "d.dat" FOR RANDOM AS #1 LEN = LEN(wow)
PUT #1, , wow
CLOSE #1
OPEN files$ + "c.dat" FOR RANDOM AS #1 LEN = LEN(ir(0, 0))
FOR x = 0 TO 54
    FOR y = 0 TO 9
        PUT #1, , ir(x, y)
NEXT y: NEXT x
FOR x = 0 TO 54
    FOR y = 0 TO 9
        PUT #1, , ifar(x, y)
NEXT y: NEXT x
FOR x = 0 TO 54
    FOR y = 0 TO 9
        PUT #1, , isar(x, y)
NEXT y: NEXT x
CLOSE #1
FOR level = 0 TO 600
    IF adc(14, level) < sample& THEN GOTO 1949
    FOR x = 0 TO 9
        FOR y = 0 TO 33
            IF m(level, x, y) > 2 AND m(level, x, y) < 6 THEN m(level, x, y) = 3: REM eliminating second response option
            IF m(level, x, y) > 5 AND m(level, x, y) < 9 THEN m(level, x, y) = 4: REM code for DD is 3, Split is 4, and Surrender is 5
            IF m(level, x, y) > 8 THEN m(level, x, y) = 5
            IF m(level, x, y) > 0 THEN
                IF im(x, y).nor = 9 THEN IF im(x, y).nnr <> m(level, x, y) THEN im(x, y).nor = 10: im(x, y).nnl = level: im(x, y).lr = m(level, x, y)
                IF im(x, y).nor = 8 THEN IF im(x, y).er <> m(level, x, y) THEN im(x, y).nor = 9: im(x, y).el = level: im(x, y).nnr = m(level, x, y)
                IF im(x, y).nor = 7 THEN IF im(x, y).nr <> m(level, x, y) THEN im(x, y).nor = 8: im(x, y).nl = level: im(x, y).er = m(level, x, y)
                IF im(x, y).nor = 6 THEN IF im(x, y).xr <> m(level, x, y) THEN im(x, y).nor = 7: im(x, y).xl = level: im(x, y).nr = m(level, x, y)
                IF im(x, y).nor = 5 THEN IF im(x, y).vr <> m(level, x, y) THEN im(x, y).nor = 6: im(x, y).vl = level: im(x, y).xr = m(level, x, y)
                IF im(x, y).nor = 4 THEN IF im(x, y).rr <> m(level, x, y) THEN im(x, y).nor = 5: im(x, y).rl = level: im(x, y).vr = m(level, x, y)
                IF im(x, y).nor = 3 THEN IF im(x, y).tr <> m(level, x, y) THEN im(x, y).nor = 4: im(x, y).tl = level: im(x, y).rr = m(level, x, y)
                IF im(x, y).nor = 2 THEN IF im(x, y).sr <> m(level, x, y) THEN im(x, y).nor = 3: im(x, y).sl = level: im(x, y).tr = m(level, x, y)
                IF im(x, y).nor = 1 THEN IF im(x, y).fr <> m(level, x, y) THEN im(x, y).nor = 2: im(x, y).fl = level: im(x, y).sr = m(level, x, y)
                IF im(x, y).nor = 0 THEN
                    im(x, y).nor = 1: im(x, y).fr = m(level, x, y): im(x, y).fl = 1000: im(x, y).sl = 1000: im(x, y).tl = 1000: im(x, y).rl = 1000
                    im(x, y).vl = 1000: im(x, y).xl = 1000: im(x, y).nl = 1000: im(x, y).el = 1000: im(x, y).nnl = 1000: im(x, y).ll = 1000
                END IF
            END IF
    NEXT y: NEXT x
1949 NEXT level
REM assemble tables displaying count level indices for stand, hit, double down, split and surrender
REM print tables displaying count level indices for stand, hit, double down, split and surrender
OPEN files$ + "x.txt" FOR OUTPUT AS #1
PRINT #1, "table displaying index plays"
PRINT #1, ""
dh$ = "2 3 4 5 6 7 8 9 T A "
ph$ = "05060708091011121314151617181920A9A8A7A6A5A4A3A2AA2233445566778899TT"
q$ = "   S   H   D   P   R"
FOR x = 0 TO 10
    IF x = 0 THEN PRINT #1, "     ";
    IF x > 0 THEN PRINT #1, "  ";: PRINT #1, MID$(dh$, x * 2 - 1, 2);
NEXT x
PRINT #1, ""
PRINT #1, "        "
FOR y = 0 TO 33
    valleys% = 0: peaks% = 0
    FOR x = 0 TO 9
        v% = 0: p% = 0
        IF im(x, y).nor > 1 THEN IF im(x, y).fl - 300 < 1 THEN v% = 1
        IF im(x, y).nor > 2 THEN IF im(x, y).sl - 300 < 1 THEN v% = 2
        IF im(x, y).nor > 3 THEN IF im(x, y).tl - 300 < 1 THEN v% = 3
        IF im(x, y).nor > 4 THEN IF im(x, y).rl - 300 < 1 THEN v% = 4
        IF im(x, y).nor > 5 THEN IF im(x, y).vl - 300 < 1 THEN v% = 5
        IF im(x, y).nor > 6 THEN IF im(x, y).xl - 300 < 1 THEN v% = 6
        IF im(x, y).nor > 7 THEN IF im(x, y).nl - 300 < 1 THEN v% = 7
        IF im(x, y).nor > 8 THEN IF im(x, y).el - 300 < 1 THEN v% = 8
        IF im(x, y).nor > 9 THEN IF im(x, y).nnl - 300 < 1 THEN v% = 9
        IF im(x, y).nor > 1 THEN IF im(x, y).fl - 300 > 0 THEN p% = 1
        IF im(x, y).nor > 2 THEN IF im(x, y).sl - 300 > 0 THEN p% = 2
        IF im(x, y).nor > 3 THEN IF im(x, y).tl - 300 > 0 THEN p% = 3
        IF im(x, y).nor > 4 THEN IF im(x, y).rl - 300 > 0 THEN p% = 4
        IF im(x, y).nor > 5 THEN IF im(x, y).vl - 300 > 0 THEN p% = 5
        IF im(x, y).nor > 6 THEN IF im(x, y).xl - 300 > 0 THEN p% = 6
        IF im(x, y).nor > 7 THEN IF im(x, y).nl - 300 > 0 THEN p% = 7
        IF im(x, y).nor > 8 THEN IF im(x, y).el - 300 > 0 THEN p% = 8
        IF im(x, y).nor > 9 THEN IF im(x, y).nnl - 300 > 0 THEN p% = 9
        IF peaks% < p% THEN peaks% = p%
        IF valleys% < v% THEN valleys% = v%
        mp(x, y) = p%: mv(x, y) = v%
    NEXT x
    rows% = peaks% + valleys% + 1
    FOR z = rows% TO 1 STEP -1
        FOR w = 1 TO 0 STEP -1
            FOR x = 0 TO 11
                IF x = 0 AND z = valleys% + 1 THEN
                    IF w = 0 THEN PRINT #1, "    ";
                    IF w = 1 THEN
                        PRINT #1, MID$(ph$, y * 2 + 1, 2); "  ";
                    END IF
                END IF
                IF x = 0 AND z <> valleys% + 1 THEN PRINT #1, "    ";
                IF x > 0 AND x < 11 THEN
                    cell% = z + mv(x - 1, y) - valleys%
                    IF cell% < 1 THEN PRINT #1, "    ";
                    IF rows% = 1 THEN
                        IF w = 0 THEN PRINT #1, "    ";
                        IF w = 1 THEN
                            PRINT #1, MID$(q$, im(x - 1, y).fr * 4 - 3, 4);
                        END IF
                    END IF
                    IF rows% <> 1 THEN
                        IF cell% = 1 THEN
                            IF w = 0 THEN PRINT #1, "    ";
                            IF w = 1 THEN
                                PRINT #1, MID$(q$, im(x - 1, y).fr * 4 - 3, 4);
                            END IF
                        END IF
                        IF cell% = 2 THEN
                            IF im(x - 1, y).fl <> 1000 THEN
                                IF w = 0 THEN
                                    PRINT #1, USING "####"; im(x - 1, y).fl - 300;
                                END IF
                                IF w = 1 THEN
                                    PRINT #1, MID$(q$, im(x - 1, y).sr * 4 - 3, 4);
                                END IF
                            END IF
                            IF im(x - 1, y).fl = 1000 THEN PRINT #1, "    ";
                        END IF
                        IF cell% = 3 THEN
                            IF im(x - 1, y).sl <> 1000 THEN
                                IF w = 0 THEN
                                    PRINT #1, USING "####"; im(x - 1, y).sl - 300;
                                END IF
                                IF w = 1 THEN
                                    PRINT #1, MID$(q$, im(x - 1, y).tr * 4 - 3, 4);
                                END IF
                            END IF
                            IF im(x - 1, y).sl = 1000 THEN PRINT #1, "    ";
                        END IF
                        IF cell% = 4 THEN
                            IF im(x - 1, y).tl <> 1000 THEN
                                IF w = 0 THEN
                                    PRINT #1, USING "####"; im(x - 1, y).tl - 300;
                                END IF
                                IF w = 1 THEN
                                    PRINT #1, MID$(q$, im(x - 1, y).rr * 4 - 3, 4);
                                END IF
                            END IF
                            IF im(x - 1, y).tl = 1000 THEN PRINT #1, "    ";
                        END IF
                        IF cell% = 5 THEN
                            IF im(x - 1, y).rl <> 1000 THEN
                                IF w = 0 THEN
                                    PRINT #1, USING "####"; im(x - 1, y).rl - 300;
                                END IF
                                IF w = 1 THEN
                                    PRINT #1, MID$(q$, im(x - 1, y).vr * 4 - 3, 4);
                                END IF
                            END IF
                            IF im(x - 1, y).rl = 1000 THEN PRINT #1, "    ";
                        END IF
                        IF cell% = 6 THEN
                            IF im(x - 1, y).vl <> 1000 THEN
                                IF w = 0 THEN
                                    PRINT #1, USING "####"; im(x - 1, y).vl - 300;
                                END IF
                                IF w = 1 THEN
                                    PRINT #1, MID$(q$, im(x - 1, y).xr * 4 - 3, 4);
                                END IF
                            END IF
                            IF im(x - 1, y).vl = 1000 THEN PRINT #1, "    ";
                        END IF
                        IF cell% = 7 THEN
                            IF im(x - 1, y).xl <> 1000 THEN
                                IF w = 0 THEN
                                    PRINT #1, USING "####"; im(x - 1, y).xl - 300;
                                END IF
                                IF w = 1 THEN
                                    PRINT #1, MID$(q$, im(x - 1, y).nr * 4 - 3, 4);
                                END IF
                            END IF
                            IF im(x - 1, y).xl = 1000 THEN PRINT #1, "    ";
                        END IF
                        IF cell% = 8 THEN
                            IF im(x - 1, y).nl <> 1000 THEN
                                IF w = 0 THEN
                                    PRINT #1, USING "####"; im(x - 1, y).nl - 300;
                                END IF
                                IF w = 1 THEN
                                    PRINT #1, MID$(q$, im(x - 1, y).er * 4 - 3, 4);
                                END IF
                            END IF
                            IF im(x - 1, y).nl = 1000 THEN PRINT #1, "    ";
                        END IF
                        IF cell% = 9 THEN
                            IF im(x - 1, y).el <> 1000 THEN
                                IF w = 0 THEN
                                    PRINT #1, USING "####"; im(x - 1, y).el - 300;
                                END IF
                                IF w = 1 THEN
                                    PRINT #1, MID$(q$, im(x - 1, y).nnr * 4 - 3, 4);
                                END IF
                            END IF
                            IF im(x - 1, y).el = 1000 THEN PRINT #1, "    ";
                        END IF
                        IF cell% = 10 THEN
                            IF im(x - 1, y).nnl <> 1000 THEN
                                IF w = 0 THEN
                                    PRINT #1, USING "####"; im(x - 1, y).nnl - 300;
                                END IF
                                IF w = 1 THEN
                                    PRINT #1, MID$(q$, im(x - 1, y).lr * 4 - 3, 4);
                                END IF
                            END IF
                            IF im(x - 1, y).nnl = 1000 THEN PRINT #1, "    ";
                        END IF
                    END IF
                END IF
                IF x = 11 AND z = valleys% + 1 THEN
                    IF w = 0 THEN PRINT #1, "    ";
                    IF w = 1 THEN PRINT #1, "      "; MID$(ph$, y * 2 + 1, 2);
                END IF
                IF x = 11 AND z <> valleys% + 1 THEN PRINT #1, "        ";
    NEXT x: PRINT #1, " ": NEXT w: NEXT z
PRINT #1, " ": NEXT y
PRINT #1, " "
FOR x = 0 TO 10
    IF x = 0 THEN PRINT #1, "     ";
    IF x > 0 THEN PRINT #1, "  ";: PRINT #1, MID$(dh$, x * 2 - 1, 2);
NEXT x
PRINT #1, " "
PRINT #1, " "
PRINT #1, "     ";
PRINT #1, t$
PRINT #1, " "
PRINT #1, "     Key: S, H, D, P, and R are letter representing a player response options"
PRINT #1, "     S = Stand, H = Hit, D = Double Down, P = Split, R = Surrender"
PRINT #1, ""
PRINT #1, "     Numbers appearing below these letters represent card count levels."
PRINT #1, ""
PRINT #1, "     To use the table first follow the row with the label corresponding to the player hand"
PRINT #1, "     and the column with the label corresponding to the point value assigned to the dealer up card"
PRINT #1, "     to the cell containing data pertaining to this particular three card combination."
PRINT #1, ""
PRINT #1, "     Next look in this cell for the highest number that is less than or equal to the card count"
PRINT #1, ""
PRINT #1, "     Execute the instruction indicated by the letter above this number to respond to the hand."
PRINT #1, ""
PRINT #1, "     If all numbers in the cell are greater than the card count,"
PRINT #1, "     then execute the instruction that is at the bottom of the cell."
PRINT #1, ""
CLOSE #1
