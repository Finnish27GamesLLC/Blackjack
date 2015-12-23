'BJ283.BAS is a software application that enables the user to 
'program their computer to conduct high-speed game simulations
'of card games belonging to the Blackjack family of table-card games.

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

_TITLE "ULTIMA283.BAS"
TYPE rec
    dv1 AS DOUBLE
    dv2 AS DOUBLE
    dv3 AS DOUBLE
    dv4 AS DOUBLE
    dv5 AS DOUBLE
    dv6 AS DOUBLE
    dv7 AS DOUBLE
    dv8 AS DOUBLE
    dv9 AS DOUBLE
    dv10 AS DOUBLE
    dv11 AS DOUBLE
    dv12 AS DOUBLE
    dv13 AS DOUBLE
    fv1 AS DOUBLE
    fv2 AS DOUBLE
    fv3 AS DOUBLE
    fv4 AS DOUBLE
    fv5 AS DOUBLE
    fv6 AS DOUBLE
    fv7 AS DOUBLE
    fv8 AS DOUBLE
    fv9 AS DOUBLE
    fv10 AS DOUBLE
    fv11 AS DOUBLE
    fv12 AS DOUBLE
    fv13 AS DOUBLE
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
DIM c(1): DIM sd AS bsrules: DIM num AS bs: DIM ez AS rec
DIM Hit(54, 9): DIM sar(54, 9) AS INTEGER: DIM far(54, 9) AS INTEGER: DIM r(54, 9) AS INTEGER
DIM dd(3 TO 7) AS INTEGER: DIM mc(21) AS INTEGER: DIM mv(5) AS INTEGER: DIM g(32) AS INTEGER: DIM H(32) AS INTEGER
DIM b(59) AS DOUBLE: DIM v(21): DIM cv(14): DIM phc(16, 21) AS INTEGER: DIM dc(21) AS INTEGER: DIM dh(1) AS INTEGER
DIM card(16) AS INTEGER: DIM mc21po(13, 2) AS DOUBLE: DIM ap(6, 2) AS INTEGER: DIM stack(416) AS INTEGER: DIM sample(1000) AS DOUBLE
DIM dv(13) AS DOUBLE: DIM ev(13) AS DOUBLE: DIM fv(13) AS DOUBLE: DIM gv(14) AS DOUBLE: DIM av(416) AS INTEGER
DIM denom(416) AS INTEGER: DIM suit(416) AS INTEGER: DIM suitp(6) AS INTEGER: DIM pdenom(12) AS INTEGER
DIM psc(16, 21) AS INTEGER: DIM dcs(21) AS INTEGER: DIM phcd(16, 21) AS INTEGER: DIM dcd(21) AS INTEGER
DIM ptm(3) AS INTEGER: DIM order1(10000) AS INTEGER: DIM rank1(10000) AS INTEGER: DIM ld(10000) AS DOUBLE
DIM id(10000) AS DOUBLE: DIM idr(10000) AS LONG: DIM ide(10000) AS DOUBLE: DIM idf(10000) AS LONG
GOTO 543
987 c1% = FIX(RND * card(13))
c% = stack(c1%): card(13) = card(13) - 1: card(denom(c%)) = card(denom(c%)) - 1: stack(c1%) = stack(card(13)): stack(card(13)) = c%
RETURN
707 FOR x = 0 TO 13
card(x) = deck% * mc(x): NEXT x
323 cv(13) = 0: ev(13) = 0: gv(14) = 0
FOR x = 0 TO 12
    IF fv(x) <> 0 AND ezc% = 1 THEN cv(x) = dv(x) / fv(x) * 100
    cv(13) = cv(13) - cv(x) * card(x): ev(13) = ev(13) + cv(x) * mc(x) / 4: gv(14) = gv(14) + ABS(cv(x)) * mc(x) / 4
NEXT x
IF ezc% = 1 AND bal% = 1 THEN
    IF balance% = 1 THEN
        cv(13) = 0
        FOR x = 0 TO 12
            gv(x) = cv(x) - ev(13) * mc(x) / mc(13)
            cv(13) = cv(13) - gv(x) * card(x)
        NEXT x
    END IF
    IF balance% = 2 THEN
        cv(13) = 0
        FOR x = 0 TO 12
            IF cv(x) >= 0 THEN gv(x) = cv(x) * gv(14) / (gv(14) + ev(13))
            IF cv(x) < 0 THEN gv(x) = cv(x) * gv(14) / (gv(14) - ev(13))
            cv(13) = cv(13) - gv(x) * card(x)
        NEXT x
    END IF
END IF
standin = cv(13): IF true% = 1 THEN standin = standin / CINT(card(13) / mc(13)): IF standin > 0 THEN standin = FIX(standin)
b(7) = standin + constant
IF truequit% <> 1 THEN IF quit > b(7) THEN b(14) = b(14) + 1: GOTO 707
IF truequit% = 1 THEN IF quit * card(13) / (mc(13) * deck%) > b(7) THEN b(14) = b(14) + 1: GOTO 707
IF reshf < b(7) THEN b(14) = b(14) + 1: GOTO 707
IF card(13) < shf * deck% * mc(13) / 100 THEN b(14) = b(14) + 1: GOTO 707
IF expo% = 1 THEN b(9) = multiplier * factor ^ b(7)
IF multi% = 1 THEN b(9) = factor * b(7)
IF b(9) < 1000000000 THEN bet& = INT(b(9)) ELSE bet& = 1000000000
IF limpoi% = 1 THEN
    IF bet& < sml& THEN bet& = sml&
    IF bet& > lrg& THEN bet& = lrg&
END IF
IF limper% = 1 THEN
    IF bet& < lobet * b(2) / 100 THEN bet& = INT(lobet * b(2) / 100)
    IF bet& > hibet * b(2) / 100 THEN bet& = INT(hibet * b(2) / 100)
END IF
IF pbs% = 1 THEN bet& = INT(bet& * b(37) / pbs)
IF propor% = 1 THEN bet& = INT(bet& * b(2) / pbs)
bet& = INT(bet& / mult%) * mult%
IF bet& < mini& THEN bet& = mini&
IF bet& > maxi& THEN bet& = maxi&
b(21) = bet&
b(28) = b(28) + card(13): b(38) = b(38) + 1
FOR x = 1 TO 2
    FOR y = 0 TO ap%
GOSUB 987: ap(y, x) = c%: NEXT y: NEXT x
GOSUB 987: dcd(1) = denom(c%): dc(1) = av(c%): dcs(1) = suit(c%): dc(0) = 1
GOSUB 987: dcd(2) = denom(c%): dc(2) = av(c%): dcs(2) = suit(c%): dc(0) = 2
IF bet& = 0 THEN GOTO 323
IF dc(1) <= dc(2) THEN dn = dc(1) / 2 * (19 - dc(1)) + dc(2)
IF dc(2) < dc(1) THEN dn = dc(2) / 2 * (19 - dc(2)) + dc(1)
b(8) = b(8) + b(7): b(10) = b(10) + b(7) * b(21): b(11) = b(11) + b(21): b(34) = 0: b(35) = 0
IF ins% = 1 THEN
    IF insmode% = 0 THEN insurance% = 0
    IF insmode% = 1 THEN
        IF (card(9) + card(10) + card(11) + card(12)) / card(13) > 1 / 3 THEN b(58) = b(58) + b(7): b(59) = b(59) + 1
        IF b(7) > b(58) / (b(59) + 1E-08) AND dc(1) = 0 THEN insurance% = 1 ELSE insurance% = 0
    END IF
    IF insmode% = 2 THEN insurance% = 1
END IF
FOR z = 0 TO ap%
    phcd(1, 1) = denom(ap(z, 1)): phcd(1, 2) = denom(ap(z, 2)): phc(1, 1) = av(ap(z, 1)): phc(1, 2) = av(ap(z, 2))
    psc(1, 1) = suit(ap(z, 1)): psc(1, 2) = suit(ap(z, 2)): surrender% = 0
    IF phc(1, 1) <= phc(1, 2) THEN n = phc(1, 1) / 2 * (19 - phc(1, 1)) + phc(1, 2)
    IF phc(1, 2) < phc(1, 1) THEN n = phc(1, 2) / 2 * (19 - phc(1, 2)) + phc(1, 1)
    REM IF z <> ap% THEN GOTO 98
    IF z = ap% THEN
        n% = n: b(5) = Hit(n, dc(1)): b(4) = b(4) + b(5): b(6) = b(4) / (rn&& + 1)
        pc1% = phcd(1, 1): pc2% = phcd(1, 2): ps1% = psc(1, 1): ps2% = psc(1, 2)
    END IF
    phc(0, 0) = 1: phc(0, 1) = 2: hand% = 1: ace% = phc(1, 1) * phc(1, 2): phc(1, 0) = phc(1, 1) + phc(1, 2): v(1) = 2
    IF ace% = 0 THEN phc(1, 0) = phc(1, 0) + 10
    IF n = 9 THEN v(1) = blackjack * 2: GOTO 7
    IF early% = 1 AND (r(n, dc(1)) = 31 AND ls% = 1) THEN surrender% = 1: GOTO 7
    IF hole% <> 1 THEN
        IF (dc(1) = 9 AND dc(2) = 0) AND ten% <> 1 THEN GOTO 7
        IF (dc(1) = 0 AND dc(2) = 9) AND push% <> 1 THEN GOTO 7
    END IF
    IF (r(n, dc(1)) = 31 AND ls% = 1) THEN v(1) = 1: surrender% = 1: GOTO 7
    IF ace% = 0 THEN phc(1, 0) = phc(1, 0) - 10
    1 DO WHILE rsa% > phc(0, 0) AND ace% = 0 AND r(n, dc(1)) = 30
        phc(0, 0) = phc(0, 0) + 1: phc(phc(0, 0), 1) = phc(hand%, 2): phcd(phc(0, 0), 1) = phcd(hand%, 2): psc(phc(0, 0), 1) = psc(hand%, 2)
        GOSUB 987
        phcd(hand%, 2) = denom(c%): phc(hand%, 2) = av(c%): psc(hand%, 2) = suit(c%): phc(hand%, 0) = phc(hand%, 1) + phc(hand%, 2): ace% = phc(hand%, 1) * phc(hand%, 2)
        IF phc(hand%, 1) <= phc(hand%, 2) THEN n = phc(hand%, 1) / 2 * (19 - phc(hand%, 1)) + phc(hand%, 2) ELSE n = phc(hand%, 2) / 2 * (19 - phc(hand%, 2)) + phc(hand%, 1)
    LOOP
    DO WHILE most% > phc(0, 0) AND ace% <> 0 AND r(n, dc(1)) = 30
        phc(0, 0) = phc(0, 0) + 1: phc(phc(0, 0), 1) = phc(hand%, 2): phcd(phc(0, 0), 1) = phcd(hand%, 2): psc(phc(0, 0), 1) = psc(hand%, 2)
        GOSUB 987
        phcd(hand%, 2) = denom(c%): phc(hand%, 2) = av(c%): psc(hand%, 2) = suit(c%): phc(hand%, 0) = phc(hand%, 1) + phc(hand%, 2): ace% = phc(hand%, 1) * phc(hand%, 2)
        IF phc(hand%, 1) <= phc(hand%, 2) THEN n = phc(hand%, 1) / 2 * (19 - phc(hand%, 1)) + phc(hand%, 2) ELSE n = phc(hand%, 2) / 2 * (19 - phc(hand%, 2)) + phc(hand%, 1)
    LOOP
    IF phc(0, 0) = 1 THEN op% = r(n, dc(1)) ELSE IF phc(hand%, 1) > phc(hand%, 2) THEN op% = far(n, dc(1)) ELSE op% = sar(n, dc(1))
    IF phc(0, 0) <> 1 AND phc(1, 1) = 0 AND dsa% <> 1 THEN op% = 0
    IF (op% > 0 AND op% < 30) THEN
        softhand% = 0
        DO WHILE phc(hand%, 0) + phc(0, hand%) < H(op%) + 2 AND op% <> 32
            GOSUB 987: softhand% = 1
            phc(0, hand%) = phc(0, hand%) + 1: phcd(hand%, phc(0, hand%)) = denom(c%): phc(hand%, phc(0, hand%)) = av(c%): psc(hand%, phc(0, hand%)) = suit(c%)
            phc(hand%, 0) = phc(hand%, 0) + av(c%): ace% = ace% * av(c%)
            IF phc(hand%, 0) + phc(0, hand%) < 12 THEN
                IF phc(0, hand%) < 8 THEN
                    IF dd(phc(0, hand%)) = 1 THEN
                        IF (schw% = 1 AND phc(0, hand%) < 5) OR schw% <> 1 THEN
                            IF phc(hand%, 1) > phc(hand%, 2) THEN
                                n = phc(hand%, 2) / 2 * (19 - phc(hand%, 2)) + phc(hand%, 0) + phc(0, hand%) - phc(hand%, 2) - 2: REM subtract 2 because code for each card value is -1 actual card value
                                IF phc(0, 0) = 1 THEN op% = r(n, dc(1)) ELSE op% = far(n, dc(1))
                                IF (op% = 30 OR op% = 31) THEN op% = far(n, dc(1))
                            END IF
                            IF phc(hand%, 1) <= phc(hand%, 2) THEN
                                n = phc(hand%, 1) / 2 * (19 - phc(hand%, 1)) + phc(hand%, 0) + phc(0, hand%) - phc(hand%, 1) - 2
                                IF phc(0, 0) = 1 THEN op% = r(n, dc(1)) ELSE op% = far(n, dc(1))
                                IF (op% = 30 OR op% = 31) THEN op% = far(n, dc(1))
                            END IF
                        END IF
                    END IF
                END IF
            END IF
        LOOP
        IF (ace% = 0 AND op% <> 32 AND phc(hand%, 0) + phc(0, hand%) < 12) THEN phc(hand%, 0) = phc(hand%, 0) + 10
        DO WHILE phc(hand%, 0) + phc(0, hand%) < g(op%) AND op% <> 32
            IF softhand% = 0 THEN
                GOSUB 987
                phc(0, hand%) = phc(0, hand%) + 1: phcd(hand%, phc(0, hand%)) = denom(c%): phc(hand%, phc(0, hand%)) = av(c%): psc(hand%, phc(0, hand%)) = suit(c%)
                phc(hand%, 0) = phc(hand%, 0) + av(c%): ace% = ace% * av(c%)
            END IF
            softhand% = 0
            IF phc(0, hand%) < 8 THEN
                IF dd(phc(0, hand%)) = 1 THEN
                    IF (schw% = 1 AND phc(0, hand%) < 5) OR schw% <> 1 THEN
                        IF phc(hand%, 1) > phc(hand%, 2) THEN
                            ppn = phc(hand%, 0) + phc(0, hand%) - 10
                            IF ace% <> 0 AND ppn = 1 THEN
                                ppn = 2
                            END IF
                            IF ppn < phc(hand%, 2) THEN ppn = phc(hand%, 2)
                            n = ppn / 2 * (19 - ppn) + phc(hand%, 0) + phc(0, hand%) - ppn - 2: REM subtract 2 because code for each card value is -1 actual card value
                            IF phc(0, 0) = 1 THEN op% = r(n, dc(1)) ELSE op% = far(n, dc(1))
                            IF (op% = 30 OR op% = 31) THEN op% = far(n, dc(1))
                        END IF
                        IF phc(hand%, 1) <= phc(hand%, 2) THEN
                            ppn = phc(hand%, 0) + phc(0, hand%) - 10
                            IF ace% <> 0 AND ppn = 1 THEN
                                ppn = 2
                            END IF
                            IF ppn < phc(hand%, 1) THEN ppn = phc(hand%, 1)
                            n = ppn / 2 * (19 - ppn) + phc(hand%, 0) + phc(0, hand%) - ppn - 2
                            IF phc(0, 0) = 1 THEN op% = r(n, dc(1)) ELSE op% = far(n, dc(1))
                            IF (op% = 30 OR op% = 31) THEN op% = far(n, dc(1))
                        END IF
                    END IF
                END IF
            END IF
            IF (ace% = 0 AND op% <> 32 AND phc(hand%, 0) + phc(0, hand%) < 12) THEN phc(hand%, 0) = phc(hand%, 0) + 10
        LOOP
        softhand% = 0
    END IF
    IF (op% = 32 AND phc(0, 0) = 1) OR (op% = 32 AND das% = 1) THEN
        GOSUB 987
        phc(0, hand%) = phc(0, hand%) + 1: phc(hand%, 0) = phc(hand%, 0) + av(c%)
        ace% = ace% * av(c%): phcd(hand%, phc(0, hand%)) = denom(c%): phc(hand%, phc(0, hand%)) = av(c%): psc(hand%, phc(0, hand%)) = suit(c%): v(hand%) = 4
    END IF
    DO WHILE (schw% = 1 AND phc(0, hand%) > 6)
        card(phcd(hand%, phc(0, hand%))) = card(phcd(hand%, phc(0, hand%))) + 1
        phc(hand%, 0) = phc(hand%, 0) - phc(hand%, phc(0, hand%))
        phc(0, hand%) = phc(0, hand%) - 1: card(13) = card(13) + 1
    LOOP
    IF phc(hand%, 0) + phc(0, hand%) < 12 AND ace% = 0 THEN phc(hand%, 0) = phc(hand%, 0) + 10
    IF immed% = 1 THEN b(34) = b(34) + ABS(phc(hand%, 0) + phc(0, hand%) - 21)
    IF phc(hand%, 0) + phc(0, hand%) < 22 THEN b(35) = 1
    IF phc(0, 0) > hand% THEN
        GOSUB 987
        hand% = hand% + 1: phc(0, hand%) = 2: phcd(hand%, 2) = denom(c%): phc(hand%, 2) = av(c%): psc(hand%, 2) = suit(c%)
        phc(hand%, 0) = phc(hand%, 1) + phc(hand%, 2): ace% = phc(hand%, 1) * phc(hand%, 2): v(hand%) = 2
        IF phc(hand%, 1) <= phc(hand%, 2) THEN n = phc(hand%, 1) / 2 * (19 - phc(hand%, 1)) + phc(hand%, 2) ELSE n = phc(hand%, 2) / 2 * (19 - phc(hand%, 2)) + phc(hand%, 1)
        IF bjsh% = 1 AND n = 9 THEN v(hand%) = blackjack * 2
        GOTO 1
    END IF
    7 FOR x = 1 TO phc(0, 0)
        noc&& = noc&& + phc(0, x): noh&& = noh&& + 1
    NEXT x
NEXT z
77 dace% = 1: dh(1) = 0
FOR x = 1 TO dc(0)
dh(1) = dh(1) + dc(x): dace% = dace% * dc(x): NEXT x: dh(1) = dh(1) + dc(0)
IF dace% = 0 AND dh(1) > j + 1 AND dh(1) < 12 THEN dh(1) = dh(1) + 10: GOTO 8
IF dc(0) <> 1 THEN IF (n = 9 AND phc(0, 0) = 1 AND ap% = 0) OR dh(1) > 16 THEN GOTO 8
IF immed% = 1 AND b(34) = 0 THEN GOTO 8
IF dbc% <> 1 AND dbc% <> 2 AND b(35) = 0 THEN GOTO 8
dc(0) = dc(0) + 1
GOSUB 987: REM dealer hand
dcd(dc(0)) = denom(c%): dc(dc(0)) = av(c%): dcs(dc(0)) = suit(c%): dh(1) = 0: GOTO 77
8 IF dace% = 0 AND dh(1) < 12 THEN dh(1) = dh(1) + 10
noc&& = noc&& + dc(0): noh&& = noh&& + 1
IF dc(1) = 0 AND dc(2) < 9 THEN b(25) = insurance% * bet&
IF dc(1) = 0 AND dc(2) = 9 THEN b(23) = 2 * insurance% * bet&
IF early% = 1 THEN IF surrender% = 1 THEN b(25) = b(25) + v(1) * bet&: b(27) = b(27) + v(1): b(0) = b(0) + v(1): GOTO 9
IF hole% = 1 OR push% = 1 THEN GOTO 85
IF dn = 9 AND (n = 9 AND phc(0, 0) = 1) THEN
    IF pbd% = 1 THEN b(23) = b(23) + v(1) * bet&: b(26) = b(26) + v(1): mc21po(13, 1) = mc21po(13, 1) + 1
    IF pbd% <> 1 AND dwt% = 1 THEN b(25) = b(25) + v(1) * bet&: b(27) = b(27) + v(1)
    IF pbd% <> 1 AND dwt% <> 1 THEN b(0) = b(0) + v(1)
    b(3) = b(3) + v(1)
    GOTO 9
END IF
IF dn = 9 AND NOT (n = 9 AND phc(0, 0) = 1) THEN b(25) = b(25) + v(1) * bet&: b(27) = b(27) + v(1): GOTO 9
IF n = 9 AND phc(0, 0) = 1 AND dn <> 9 THEN
    b(23) = b(23) + v(1) * bet&
    b(26) = b(26) + v(1)
    mc21po(13, 1) = mc21po(13, 1) + 1
    GOTO 9
END IF
85 FOR x = 1 TO phc(0, 0)
    IF phc(x, 1) < phc(x, 2) THEN n = phc(x, 1) / 2 * (19 - phc(x, 1)) + phc(x, 2) ELSE n = phc(x, 2) / 2 * (19 - phc(x, 2)) + phc(x, 1)
    IF push% <> 1 AND (dc(1) = 0 AND dc(2) = 9) AND n <> 9 THEN b(25) = b(25) + v(x) * bet&: b(27) = b(27) + v(x): GOTO 81
    IF ten% <> 1 AND (dc(1) = 9 AND dc(2) = 0) AND n <> 9 THEN b(25) = b(25) + v(x) * bet&: b(27) = b(27) + v(x): GOTO 81
    IF early% <> 1 THEN IF surrender% = 1 THEN b(25) = b(25) + bet&: b(27) = b(27) + 1: b(0) = b(0) + 1: GOTO 9
    IF bjsh% = 1 AND n = 9 THEN
        v(x) = blackjack * 2: b(23) = b(23) + v(x) * bet&: b(26) = b(26) + v(x)
        mc21po(12, 1) = mc21po(12, 1) + 1
        GOTO 81
    END IF
    IF schw% = 1 AND phc(0, x) > 5 AND phc(x, 0) + phc(0, x) < 22 THEN
        b(23) = b(23) + v(x) * bet&: b(26) = b(26) + v(x)
        GOTO 81: REM Automatic Six Card Hand Win Here
    END IF
    IF b21po% = 1 AND phc(x, 0) + phc(0, x) = 21 THEN
        IF phc(0, x) = 3 THEN
            IF (phc(x, 1) + 1) * (phc(x, 2) + 1) * (phc(x, 3) + 1) = 343 THEN
                op% = 1
                IF (psc(x, 1) = psc(x, 2) AND psc(x, 2) = psc(x, 3)) THEN IF psc(x, 1) <> 6 THEN op% = 2 ELSE op% = 3
                IF (op% > 1 AND dc(1) = 6 AND dc(2) = 6) THEN
                    IF phc(0, 0) = 1 THEN op% = 4
                END IF
                IF op% = 1 AND ms777 <> 0 THEN
                    IF ptm(3) <> 1 THEN v(x) = v(x) + ms777 * 2: mc21po(1, 1) = mc21po(1, 1) + 1: b(23) = b(23) + v(x) * bet&: b(26) = b(26) + v(x): GOTO 81
                    IF ptm(3) = 1 THEN mc21po(1, 1) = mc21po(1, 1) + 1: b(23) = b(23) + v(x) * bet& + mini& * 2 * ms777: b(26) = b(26) + v(x) + 2 * ms777: GOTO 81
                END IF
                IF op% = 2 AND ss777 <> 0 THEN
                    IF ptm(3) <> 1 THEN v(x) = v(x) + ss777 * 2: mc21po(2, 1) = mc21po(2, 1) + 1: b(23) = b(23) + v(x) * bet&: b(26) = b(26) + v(x): GOTO 81
                    IF ptm(3) = 1 THEN mc21po(2, 1) = mc21po(2, 1) + 1: b(23) = b(23) + v(x) * bet& + mini& * 2 * ss777: b(26) = b(26) + v(x) + 2 * ss777: GOTO 81
                END IF
                IF op% = 3 AND sp777 <> 0 THEN
                    IF ptm(3) <> 1 THEN v(x) = v(x) + sp777 * 2: mc21po(3, 1) = mc21po(3, 1) + 1: b(23) = b(23) + v(x) * bet&: b(26) = b(26) + v(x): GOTO 81
                    IF ptm(3) = 1 THEN mc21po(3, 1) = mc21po(3, 1) + 1: b(23) = b(23) + v(x) * bet& + mini& * 2 * sp777: b(26) = b(26) + v(x) + 2 * sp777: GOTO 81
                END IF
                IF op% = 4 AND ptm(3) <> 1 THEN
                    v(x) = v(x) + jp777 * 2: mc21po(4, 1) = mc21po(11, 1) + 1: b(23) = b(23) + v(x) * bet&: b(26) = b(26) + v(x): GOTO 81
                END IF
                IF op% = 4 AND ptm(3) = 1 THEN
                    IF bet& < 5 * mini& THEN mc21po(2, 1) = mc21po(2, 1) + 1: b(23) = b(23) + v(x) * bet& + mini& * 2 * ss777: b(26) = b(26) + v(x) + 2 * ss777: GOTO 81
                    IF bet& = 5 * mini& THEN mc21po(11, 1) = mc21po(11, 1) + 1: b(23) = b(23) + v(x) * bet& + mini& * 2 * jp777: b(26) = b(26) + v(x) + 2 * jp777: GOTO 81
                    IF bet& > 5 * mini& THEN mc21po(11, 1) = mc21po(11, 1) + 1: b(23) = b(23) + v(x) * bet& + mini& * jp777: b(26) = b(26) + v(x) + 2 * jp777: GOTO 81
                END IF
            END IF
            IF (phc(x, 1) + 1) * (phc(x, 2) + 1) * (phc(x, 3) + 1) = 336 THEN
                op% = 8
                IF (psc(x, 1) = psc(x, 2) AND psc(x, 2) = psc(x, 3)) THEN IF psc(x, 1) <> 6 THEN op% = 9 ELSE op% = 10
                IF op% = 8 AND ms678 <> 0 THEN
                    IF ptm(2) <> 1 THEN v(x) = v(x) + ms678 * 2: mc21po(8, 1) = mc21po(8, 1) + 1: b(23) = b(23) + v(x) * bet&: b(26) = b(26) + v(x): GOTO 81
                    IF ptm(2) = 1 THEN mc21po(8, 1) = mc21po(8, 1) + 1: b(23) = b(23) + v(x) * bet& + mini& * 2 * ms678: b(26) = b(26) + v(x) + 2 * ms678: GOTO 81
                END IF
                IF op% = 9 AND ss678 <> 0 THEN
                    IF ptm(2) <> 1 THEN v(x) = v(x) + ss678 * 2: mc21po(9, 1) = mc21po(9, 1) + 1: b(23) = b(23) + v(x) * bet&: b(26) = b(26) + v(x): GOTO 81
                    IF ptm(2) = 1 THEN mc21po(8, 1) = mc21po(9, 1) + 1: b(23) = b(23) + v(x) * bet& + mini& * 2 * ss678: b(26) = b(26) + v(x) + 2 * ss678: GOTO 81
                END IF
                IF op% = 10 AND sp678 <> 0 THEN
                    IF ptm(2) <> 1 THEN v(x) = v(x) + sp678 * 2: mc21po(10, 1) = mc21po(10, 1) + 1: b(23) = b(23) + v(x) * bet&: b(26) = b(26) + v(x): GOTO 81
                    IF ptm(2) = 1 THEN mc21po(8, 1) = mc21po(8, 1) + 1: b(23) = b(23) + v(x) * bet& + mini& * 2 * sp678: b(26) = b(26) + v(x) + 2 * sp678: GOTO 81
                END IF
            END IF
        END IF
        IF phc(0, x) = 6 AND v(x) <> 4 AND mc21po(4, 0) <> 0 THEN
            IF ptm(1) <> 1 THEN v(x) = v(x) + mc21po(4, 0) * 2: mc21po(4, 1) = mc21po(4, 1) + 1: b(23) = b(23) + v(x) * bet&: b(26) = b(26) + v(x): GOTO 81
            IF ptm(1) = 1 THEN mc21po(4, 1) = mc21po(4, 1) + 1: b(23) = b(23) + v(x) * bet& + mini& * 2 * mc21po(4, 0): b(26) = b(26) + v(x) + 2 * mc21po(4, 0): GOTO 81
        END IF
        IF phc(0, x) = 7 AND v(x) <> 4 AND mc21po(5, 0) <> 0 THEN
            IF ptm(1) <> 1 THEN v(x) = v(x) + mc21po(5, 0) * 2: mc21po(5, 1) = mc21po(5, 1) + 1: b(23) = b(23) + v(x) * bet&: b(26) = b(26) + v(x): GOTO 81
            IF ptm(1) = 1 THEN mc21po(5, 1) = mc21po(5, 1) + 1: b(23) = b(23) + v(x) * bet& + mini& * 2 * mc21po(5, 0): b(26) = b(26) + v(x) + 2 * mc21po(5, 0): GOTO 81
        END IF
        IF phc(0, x) = 8 AND v(x) <> 4 AND mc21po(6, 0) <> 0 THEN
            IF ptm(1) <> 1 THEN v(x) = v(x) + mc21po(6, 0) * 2: mc21po(6, 1) = mc21po(6, 1) + 1: b(23) = b(23) + v(x) * bet&: b(26) = b(26) + v(x): GOTO 81
            IF ptm(1) = 1 THEN mc21po(6, 1) = mc21po(6, 1) + 1: b(23) = b(23) + v(x) * bet& + mini& * 2 * mc21po(6, 0): b(26) = b(26) + v(x) + 2 * mc21po(6, 0): GOTO 81
        END IF
        IF phc(0, x) = 9 AND v(x) <> 4 AND mc21po(7, 0) <> 0 THEN
            IF ptm(1) <> 1 THEN v(x) = v(x) + mc21po(7, 0) * 2: mc21po(7, 1) = mc21po(7, 1) + 1: b(23) = b(23) + v(x) * bet&: b(26) = b(26) + v(x): GOTO 81
            IF ptm(1) = 1 THEN mc21po(7, 1) = mc21po(7, 1) + 1: b(23) = b(23) + v(x) * bet& + mini& * 2 * mc21po(7, 0): b(26) = b(26) + v(x) + 2 * mc21po(7, 0): GOTO 81
        END IF
    END IF
    IF (phc(x, 0) + phc(0, x) > dh(1) AND phc(x, 0) + phc(0, x) < 22) THEN b(23) = b(23) + v(x) * bet&: b(26) = b(26) + v(x)
    IF (dh(1) > phc(x, 0) + phc(0, x) AND dh(1) < 22) THEN b(25) = b(25) + v(x) * bet&: b(27) = b(27) + v(x)
    IF (dh(1) > 21 AND phc(x, 0) + phc(0, x) < 22) THEN b(23) = b(23) + v(x) * bet&: b(26) = b(26) + v(x): b(20) = b(20) + v(x)
    IF (phc(x, 0) + phc(0, x) > 21 AND dh(1) < 22) THEN b(25) = b(25) + v(x) * bet&: b(27) = b(27) + v(x): b(19) = b(19) + v(x)
    IF (phc(x, 0) + phc(0, x) > 21 AND dh(1) > 21) THEN
        IF (dbc% <> 1 AND dbc% <> 2) THEN b(25) = b(25) + v(x) * bet&: b(27) = b(27) + v(x): b(46) = b(46) + v(x)
        IF (dbc% = 1 OR dbc% = 2) THEN
            IF phc(x, 0) + phc(0, x) > dh(1) THEN b(25) = b(25) + v(x) * bet&: b(27) = b(27) + v(x): b(46) = b(46) + v(x)
            IF phc(x, 0) + phc(0, x) = dh(1) THEN
                IF dwt% <> 1 THEN
                    IF dbc% = 1 THEN b(25) = b(25) + v(x) * .5 * bet&: b(27) = b(27) + v(x) * .5: b(46) = b(46) + v(x) * .5: b(0) = b(0) + v(x) * .5: b(1) = b(1) + v(x) * .5
                    IF dbc% = 2 THEN b(0) = b(0) + v(x): b(1) = b(1) + v(x)
                END IF
                IF dwt% = 1 THEN b(25) = b(25) + v(x) * bet&: b(27) = b(27) + v(x): b(46) = b(46) + v(x)
            END IF
            IF phc(x, 0) + phc(0, x) < dh(1) THEN
                IF dbc% = 1 AND phc(x, 0) + phc(0, x) < dh(1) THEN b(0) = b(0) + v(x): b(1) = b(1) + v(x)
                IF dbc% = 2 THEN b(23) = b(23) + v(x) * bet&: b(26) = b(26) + v(x): b(45) = b(45) + v(x)
            END IF
        END IF
    END IF
    IF (phc(x, 0) + phc(0, x) = dh(1) AND dh(1) < 21) THEN
        IF dwt% = 1 THEN b(25) = b(25) + v(x) * bet&: b(27) = b(27) + v(x)
        IF dwt% <> 1 THEN b(0) = b(0) + v(x)
    END IF
    IF (phc(x, 0) + phc(0, x) = 21 AND dh(1) = 21) THEN
        IF immed% = 1 THEN b(23) = b(23) + v(x) * bet&: b(26) = b(26) + v(x)
        IF immed% <> 1 AND dwt% = 1 THEN b(25) = b(25) + v(x) * bet&: b(27) = b(27) + v(x)
        IF immed% <> 1 AND dwt% <> 1 THEN b(0) = b(0) + v(x)
    END IF
    IF phc(x, 0) + phc(0, x) = dh(1) THEN b(3) = b(3) + v(x)
81 NEXT x
9 b(25) = b(25) + ante% * 2: b(25) = b(25) + ante% * 2 * INT((bet& - 1) / 100): b(22) = b(22) + b(23): b(24) = b(24) + b(25)
rn&& = rn&& + 1
IF ezc% = 1 THEN
    IF bottom! < b(7) OR rn&& < 11 THEN
        IF b(7) < top! OR rn&& < 11 THEN
            FOR x = 0 TO 12
                dv(x) = dv(x) + ev(x) * (b(23) - b(25))
                fv(x) = fv(x) + ABS(ev(x)) * (b(23) + b(25))
                ev(x) = (mc(x) / mc(13) - card(x) / card(13)) * card(13)
                IF true% = 1 THEN ev(x) = ev(x) * mc(13) / card(13)
            NEXT x
        END IF
    END IF
END IF
IF bet& > bigbet& THEN bigbet& = bet&
IF b(7) < b(13) THEN b(13) = b(7)
IF b(7) > b(12) THEN b(12) = b(7)
b(2) = purse + (b(22) - b(24)) / 2
IF b(2) + (b(39) - b(40)) / 2 < b(43) THEN b(43) = b(2) + (b(39) - b(40)) / 2
IF b(2) < b(36) THEN
    b(36) = b(2)
    b(15) = rn&& - b(16) * scr&
END IF
IF b(2) < purse + 1 THEN
    b(18) = rn&& - b(16) * scr&
END IF
IF b(2) > b(37) THEN b(37) = b(2)
IF b(2) + (b(39) - b(40)) / 2 > b(44) THEN b(44) = b(2) + (b(39) - b(40)) / 2
IF drop > b(2) - b(37) THEN drop = b(2) - b(37)
IF b(17) > b(2) - b(37) THEN b(17) = b(2) - b(37)
IF rn&& = INT(rn&& / (hmr& + 1)) * (hmr& + 1) THEN GOTO 999
IF rn&& = INT(rn&& / scr&) * scr& OR rn&& = hmr& THEN CLS ELSE GOTO 757
b(30) = rn&& / scr& - 1: b(30) = b(30) - INT(b(30) / 1000) * 1000 + 1: sample(b(30)) = b(2) - b(29): b(29) = b(2): sample(0) = 0: b(16) = b(16) + 1
IF INT(rn&& / scr&) < 1001 THEN b(30) = rn&& / scr& ELSE b(30) = 1000: REM here b(30) is equal to the number of samples used to calculate variance and standard deviation
FOR x = 1 TO b(30)
    sample(0) = sample(0) + sample(x)
NEXT x
b(31) = sample(0) / b(30): b(32) = 0: REM b(31) is equal to the mean or average change in purse b(2) after scn% of hands
FOR x = 1 TO b(30)
    b(32) = b(32) + (sample(x) - b(31)) ^ 2
NEXT x
b(32) = b(32) / b(30): REM b(32) is equal to variance square root of b(32) is equal to one standard deviation
b(33) = b(33) + b(32): REM b(33) is equal to sum of all variance calculations
PRINT "number of cards is"; deck% * mc(13); " ";: PRINT card(13); "cards remain to be played";: PRINT "  reshuffle is at"; INT(shf / 100 * deck% * mc(13));
REM PRINT "average number of decks in play is";: PRINT USING "##.##"; b(28) / b(38) / mc(13)
PRINT "card count:";: PRINT USING "####.##"; cv(13);: IF true% = 1 THEN PRINT "  true count:";: PRINT USING "####.##"; standin;
PRINT "  constant added:";: PRINT USING "###.##"; constant;: PRINT "  bet count:";: PRINT USING "####.##"; b(7)
IF ezc% <> 1 OR z$ <> "" THEN PRINT "high count:";: PRINT USING "####.##"; b(12);: PRINT " low count:";: PRINT USING "####.##"; b(13);: PRINT " av count:";: PRINT USING "###.##"; b(8) / rn&&;
IF ezc% <> 1 OR z$ <> "" THEN PRINT " av count*wager:";: PRINT USING "###.##"; b(10) / b(11)
IF ins% = 1 THEN PRINT "insurance bet becomes profitable at a minimum count of ";: PRINT USING "###.##"; b(58) / (b(59) + 1E-09)
PRINT "table minimum is"; mini&; " table maximum is"; maxi&; ", and biggest wager yet is"; bigbet&
IF limpoi% = 1 THEN PRINT "player opts to limit bets to a spread of "; sml&; " and"; lrg&; " credits"
IF limper% = 1 THEN PRINT "player opts to limit bets to between "; lobet; "% and "; hibet; "% of purse";
IF expo% = 1 THEN PRINT multiplier;: PRINT "multiplied by ";: PRINT factor;: PRINT "raised to the power of";: PRINT USING "####.##"; b(7);: PRINT " yields a bet of"; b(21)
IF multi% = 1 THEN PRINT "a base of"; factor;: PRINT "multiplied by ";: PRINT USING "####.##"; b(7);: PRINT " yields a bet of"; b(21)
IF pbs% = 1 THEN PRINT "plateau betting multiplies by"; INT(b(37) / pbs * 100) / 100; "to yield a bet rounded to"; bet&
IF propor% = 1 THEN PRINT "proportional betting multiplies by"; INT(b(2) / pbs * 100) / 100; "to yield a bet rounded to"; bet&
PRINT "dealer upcard is "; CHR$(pdenom(dcd(1))); CHR$(dcs(1));
PRINT ", player cards are "; CHR$(pdenom(pc1%)); CHR$(ps1%); " "; CHR$(pdenom(pc2%)); CHR$(ps2%);
PRINT ", and player response is "; MID$(o$, r(n%, dc(1)) * 5 + 1, 5)
PRINT "expected value of player response is            ";: PRINT USING "####.##"; b(5) * 100;: PRINT "%"
PRINT "expected value of game is                       ";: PRINT USING "####.##"; b(6) * 100;: PRINT "%"
PRINT "dealer cards are as follows     ";
FOR x = 1 TO dc(0)
PRINT CHR$(pdenom(dcd(x))); CHR$(dcs(x)); " ";: NEXT x: PRINT " "
FOR x = 1 TO phc(0, 0)
    PRINT "player cards for hand "; x; " are   ";
    FOR y = 1 TO phc(0, x)
PRINT CHR$(pdenom(phcd(x, y))); CHR$(psc(x, y)); " ";: NEXT y: PRINT " ": NEXT x
PRINT "      player hand(s): ";
FOR x = 1 TO phc(0, 0)
    PRINT phc(x, 0) + phc(0, x); " ";
NEXT x
PRINT "                     house hand: "; dh(1)
PRINT "house won:  ";: PRINT USING "######.#"; b(25) / 2
PRINT "player won: ";: PRINT USING "######.#"; b(23) / 2
PRINT "total number of wagers won by house   :   ";: PRINT USING "##########.#"; b(27) / 2: PRINT "total number of wagers won by player  :   ";: PRINT USING "##########.#"; b(26) / 2
PRINT "player is winning ";: PRINT USING "###.##"; b(26) / (b(26) + b(27) + 1E-08) * 100;: PRINT "% of all wagers won     ROUND NUMBER "; rn&&; ""
PRINT "total number of credits won by house  : ";: PRINT USING "############.#"; b(24) / 2: PRINT "total number of credits won by player : ";: PRINT USING "############.#"; b(22) / 2
PRINT "player is winning ";: PRINT USING "###.##"; b(22) / (b(22) + b(24) + 1E-08) * 100;: PRINT "% of all credits won      reshuffles "; b(14); ""
PRINT "finish purse ";: PRINT USING "########.#"; b(2);: PRINT " high ";: PRINT USING "########.#"; b(37);
PRINT " low ";: PRINT USING "########.#"; b(36);: PRINT "  largest drop "; drop
PRINT "change in purse is an average of ";: PRINT USING "##########.##"; b(31);: PRINT " credits after "; scr&; " rounds"
PRINT "one standard deviation equal to  ";: PRINT USING "##########.##"; SQR(b(33) / INT(rn&& / scr&));: PRINT " credits"
v$ = files$ + "o.txt"
OPEN v$ FOR APPEND AS #1
PRINT #1, "number of cards is"; deck% * mc(13); " ";: PRINT #1, card(13); "cards remain to be played";: PRINT #1, "  reshuffle is at"; INT(shf / 100 * deck% * mc(13))
PRINT #1, "average number of decks in play is";: PRINT #1, USING "##.##"; b(28) / b(38) / mc(13);
PRINT #1, " average number of cards per hand is";: PRINT #1, USING "##.##"; noc&& / noh&&
PRINT #1, "card count:";: PRINT #1, USING "####.##"; cv(13);: IF true% = 1 THEN PRINT #1, "  true count:";: PRINT #1, USING "####.##"; standin;
PRINT #1, "  constant added:";: PRINT #1, USING "###.##"; constant;: PRINT #1, "  bet count:";: PRINT #1, USING "####.##"; b(7)
IF ezc% <> 1 OR z$ <> "" THEN PRINT #1, "high count:";: PRINT #1, USING "####.##"; b(12);: PRINT #1, " low count:";: PRINT #1, USING "####.##"; b(13);: PRINT #1, " av count:";: PRINT #1, USING "###.##"; b(8) / rn&&;
IF ezc% <> 1 OR z$ <> "" THEN PRINT #1, " av count*wager:";: PRINT #1, USING "###.##"; b(10) / b(11)
IF ins% = 1 THEN PRINT #1, "insurance bet becomes profitable at a minimum count of ";: PRINT #1, USING "###.##"; b(58) / (b(59) + 1E-09)
PRINT #1, "table minimum is"; mini&; " table maximum is"; maxi&; ", and biggest wager yet is"; bigbet&
IF limpoi% = 1 THEN PRINT #1, "player opts to limit bets to a spread of "; sml&; " and"; lrg&; " credits"
IF limper% = 1 THEN PRINT #1, "player opts to limit bets to between "; lobet; "% and "; hibet; "% of purse";
IF expo% = 1 THEN PRINT #1, multiplier;: PRINT #1, "multiplied by ";: PRINT #1, factor;: PRINT #1, "raised to the power of";: PRINT #1, USING "####.##"; b(7);: PRINT #1, " yields a bet of"; b(21)
IF multi% = 1 THEN PRINT #1, "a base of"; factor;: PRINT #1, "multiplied by ";: PRINT #1, USING "####.##"; b(7);: PRINT #1, " yields a bet of"; b(21)
IF pbs% = 1 THEN PRINT #1, "plateau betting multiplies by"; INT(b(37) / pbs * 100) / 100; "to yield a bet rounded to"; bet&
IF propor% = 1 THEN PRINT #1, "proportional betting multiplies by"; INT(b(2) / pbs * 100) / 100; "to yield a bet rounded to"; bet&
PRINT #1, "dealer upcard is "; CHR$(pdenom(dcd(1))); CHR$(suitp(dcs(1)));
PRINT #1, ", player cards are "; CHR$(pdenom(pc1%)); CHR$(suitp(ps1%)); " "; CHR$(pdenom(pc2%)); CHR$(suitp(ps2%));
PRINT #1, ", and player response is "; MID$(o$, r(n%, dc(1)) * 5 + 1, 5)
PRINT #1, "expected value of player response is             ";: PRINT #1, USING "####.##"; b(5) * 100;: PRINT #1, "%"
PRINT #1, "expected value of game is                        ";: PRINT #1, USING "####.##"; b(6) * 100;: PRINT #1, "%"
PRINT #1, "dealer cards are as follows     ";
FOR x = 1 TO dc(0)
PRINT #1, CHR$(pdenom(dcd(x))); CHR$(suitp(dcs(x))); " ";: NEXT x: PRINT #1, " "
FOR x = 1 TO phc(0, 0)
    PRINT #1, "player cards for hand "; x; " are   ";
    FOR y = 1 TO phc(0, x)
PRINT #1, CHR$(pdenom(phcd(x, y))); CHR$(suitp(psc(x, y))); " ";: NEXT y: PRINT #1, " ": NEXT x
PRINT #1, "      player hand(s): ";
FOR x = 1 TO phc(0, 0)
    PRINT #1, phc(x, 0) + phc(0, x); " ";
NEXT x
PRINT #1, "                     house hand: "; dh(1)
PRINT #1, "house won:  ";: PRINT #1, USING "######.#"; b(25) / 2
PRINT #1, "player won: ";: PRINT #1, USING "######.#"; b(23) / 2
PRINT #1, "total number of wagers won by house   :    ";: PRINT #1, USING "##########.#"; b(27) / 2: PRINT #1, "total number of wagers won by player  :    ";: PRINT #1, USING "##########.#"; b(26) / 2
PRINT #1, "player is winning ";: PRINT #1, USING "###.##"; b(26) / (b(26) + b(27) + 1E-08) * 100;: PRINT #1, "% of all wagers won     ROUND NUMBER "; rn&&; ""
PRINT #1, "total number of credits won by house  : ";: PRINT #1, USING "#############.#"; b(24) / 2: PRINT #1, "total number of credits won by player : ";: PRINT #1, USING "#############.#"; b(22) / 2
PRINT #1, "player is winning ";: PRINT #1, USING "###.##"; b(22) / (b(22) + b(24) + 1E-08) * 100;: PRINT #1, "% of all credits won      reshuffles "; b(14); ""
PRINT #1, "finish purse ";: PRINT #1, USING "########.#"; b(2);: PRINT #1, " high ";: PRINT #1, USING "########.#"; b(37);
PRINT #1, " low ";: PRINT #1, USING "#########.#"; b(36);: PRINT #1, "  largest drop "; drop
PRINT #1, "change in purse is an average of ";: PRINT #1, USING "##########.##"; b(31);: PRINT #1, " credits after "; scr&; " rounds"
PRINT #1, "one standard deviation equal to  ";: PRINT #1, USING "##########.##"; SQR(b(33) / INT(rn&& / scr&));: PRINT #1, " credits"
IF insurance% = 1 THEN PRINT #1, "INSURANCE WAGER ";
IF insurance% = 1 AND dc(2) = 9 THEN PRINT #1, "payed off two to one"
IF insurance% = 1 AND dc(2) < 9 THEN PRINT #1, "was taken by the house."
PRINT #1, ""
IF ezc% <> 1 THEN
    PRINT #1, "values added to the running count as each ";
    PRINT #1, "card of a given denomination was played"
    PRINT #1, "and then removed from the stack during ";
    PRINT #1, "this game simulation were as follows:"
    PRINT #1, ""
    FOR x = 0 TO 12
        PRINT #1, MID$(b$, x + 1, 1);: PRINT #1, "     = ";: PRINT #1, USING "####.###"; cv(x);
        IF bal% = 1 THEN PRINT #1, "          Balanced ";: PRINT #1, MID$(b$, x + 1, 1);: PRINT #1, "     = ";: PRINT #1, USING "####.###"; gv(x)
        IF bal% <> 1 THEN PRINT #1, ""
    NEXT x
    PRINT #1, ""
    PRINT #1, "TOTAL = ";: PRINT #1, USING "####.###"; ev(13);
    IF bal% = 1 THEN PRINT #1, "          Balanced TOTAL = ";: PRINT #1, USING "####.###"; 0
    PRINT #1, ""
END IF
IF ezc% = 1 THEN
    PRINT #1, "Change in expected value of the game wager caused"
    PRINT #1, "by the removal of one card of a given denomination "
    PRINT #1, "from the stack during this game simulation."
    PRINT #1, ""
    FOR x = 0 TO 12
        PRINT #1, MID$(b$, x + 1, 1);: PRINT #1, "     = ";: PRINT #1, USING "####.###"; cv(x);
        IF bal% = 1 THEN PRINT #1, "          Balanced ";: PRINT #1, MID$(b$, x + 1, 1);: PRINT #1, "     = ";: PRINT #1, USING "####.###"; gv(x)
        IF bal% <> 1 THEN PRINT #1, ""
    NEXT x
    PRINT #1, ""
    PRINT #1, "TOTAL = ";: PRINT #1, USING "####.###"; ev(13);
    IF bal% = 1 THEN PRINT #1, "          Balanced TOTAL = ";: PRINT #1, USING "####.###"; 0
    IF bal <> 1 THEN PRINT #1, ""
END IF
PRINT #1, ""
PRINT #1, "The user defined blackjack game rules for this simulation are as follows:"
PRINT #1, ""
PRINT #1, t$
PRINT #1, ""
PRINT #1, ""
PRINT #1, ""
PRINT #1, "The following is a table displaying the number of game wagers resolved"
PRINT #1, "by each possible outcome wherein on a win the dealer rewards the player"
PRINT #1, "on the game wager, on a loss the dealer collects the game wager from the"
PRINT #1, "the player, and on a push the dealer returns the game wager to the player."
PRINT #1, "Each surrender is counted as loss of a half wager and push of a half wager."
PRINT #1, "Each double down is counted as either win, loss, or push of two game wagers."
PRINT #1, ""
PRINT #1, ""
PRINT #1, "                      WIN           LOSS           PUSH           TOTAL"
PRINT #1, ""
b(54) = b(26) - b(45) + b(52) - b(20) + b(51)
b(55) = b(27) - b(46) + b(53) - b(19) + b(50)
b(56) = b(0) - b(47) - b(1) + b(48)
b(57) = b(54) + b(55) + b(56)
PRINT #1, "NO BUST     ";
PRINT #1, USING "###########.#"; b(54) / 2;: PRINT #1, "   ";
PRINT #1, USING "##########.#"; b(55) / 2;: PRINT #1, "   ";
PRINT #1, USING "##########.#"; b(56) / 2;: PRINT #1, "   ";
PRINT #1, USING "###########.#"; b(57) / 2;: PRINT #1, "   ";
PRINT #1, ""
b(54) = 0
b(55) = b(19) - b(50)
b(56) = 0
b(57) = b(54) + b(55) + b(56)
PRINT #1, "PLAYER BUST ";
PRINT #1, USING "###########.#"; b(54) / 2;: PRINT #1, "   ";
PRINT #1, USING "##########.#"; b(55) / 2;: PRINT #1, "   ";
PRINT #1, USING "##########.#"; b(56) / 2;: PRINT #1, "   ";
PRINT #1, USING "###########.#"; b(57) / 2;: PRINT #1, "   ";
PRINT #1, ""
b(54) = b(20) - b(51)
b(55) = 0
b(56) = 0
b(57) = b(54) + b(55) + b(56)
PRINT #1, "DEALER BUST ";
PRINT #1, USING "###########.#"; b(54) / 2;: PRINT #1, "   ";
PRINT #1, USING "##########.#"; b(55) / 2;: PRINT #1, "   ";
PRINT #1, USING "##########.#"; b(56) / 2;: PRINT #1, "   ";
PRINT #1, USING "###########.#"; b(57) / 2;: PRINT #1, "   ";
PRINT #1, ""
b(54) = b(45) - b(52)
b(55) = b(46) - b(53)
b(56) = b(1) - b(48)
b(57) = b(54) + b(55) + b(56)
PRINT #1, "DOUBLE BUST ";
PRINT #1, USING "###########.#"; b(54) / 2;: PRINT #1, "   ";
PRINT #1, USING "##########.#"; b(55) / 2;: PRINT #1, "   ";
PRINT #1, USING "##########.#"; b(56) / 2;: PRINT #1, "   ";
PRINT #1, USING "###########.#"; b(57) / 2;: PRINT #1, "   ";
PRINT #1, ""
b(54) = b(26)
b(55) = b(27)
b(56) = b(0) - b(47)
b(57) = b(54) + b(55) + b(56)
PRINT #1, "TOTAL       ";
PRINT #1, USING "###########.#"; b(54) / 2;: PRINT #1, "   ";
PRINT #1, USING "##########.#"; b(55) / 2;: PRINT #1, "   ";
PRINT #1, USING "##########.#"; b(56) / 2;: PRINT #1, "   ";
PRINT #1, USING "###########.#"; b(57) / 2;: PRINT #1, "   ";
PRINT #1, ""
PRINT #1, ""
PRINT #1, ""
PRINT #1, "The following is a table displaying percentage of game wagers resolved"
PRINT #1, "by each possible outcome"
PRINT #1, ""
PRINT #1, ""
PRINT #1, "                      WIN           LOSS           PUSH           TOTAL"
PRINT #1, ""
b(54) = b(26) - b(45) + b(52) - b(20) + b(51)
b(55) = b(27) - b(46) + b(53) - b(19) + b(50)
b(56) = b(0) - b(47) - b(1) + b(48)
b(57) = b(54) + b(55) + b(56)
b(54) = b(54) / (b(26) + b(27) + b(0) - b(47)) * 100
b(55) = b(55) / (b(26) + b(27) + b(0) - b(47)) * 100
b(56) = b(56) / (b(26) + b(27) + b(0) - b(47)) * 100
b(57) = b(54) + b(55) + b(56)
PRINT #1, "NO BUST     ";
PRINT #1, USING "##########.##"; b(54);: PRINT #1, "%  ";
PRINT #1, USING "#########.##"; b(55);: PRINT #1, "%  ";
PRINT #1, USING "#########.##"; b(56);: PRINT #1, "%  ";
PRINT #1, USING "##########.##"; b(57);: PRINT #1, "%  ";
PRINT #1, ""
b(54) = 0
b(55) = b(19) - b(50)
b(56) = 0
b(57) = b(54) + b(55) + b(56)
b(54) = b(54) / (b(26) + b(27) + b(0) - b(47)) * 100
b(55) = b(55) / (b(26) + b(27) + b(0) - b(47)) * 100
b(56) = b(56) / (b(26) + b(27) + b(0) - b(47)) * 100
b(57) = b(54) + b(55) + b(56)
PRINT #1, "PLAYER BUST ";
PRINT #1, USING "##########.##"; b(54);: PRINT #1, "%  ";
PRINT #1, USING "#########.##"; b(55);: PRINT #1, "%  ";
PRINT #1, USING "#########.##"; b(56);: PRINT #1, "%  ";
PRINT #1, USING "##########.##"; b(57);: PRINT #1, "%  ";
PRINT #1, ""
b(54) = b(20) - b(51)
b(55) = 0
b(56) = 0
b(57) = b(54) + b(55) + b(56)
b(54) = b(54) / (b(26) + b(27) + b(0) - b(47)) * 100
b(55) = b(55) / (b(26) + b(27) + b(0) - b(47)) * 100
b(56) = b(56) / (b(26) + b(27) + b(0) - b(47)) * 100
b(57) = b(54) + b(55) + b(56)
PRINT #1, "DEALER BUST ";
PRINT #1, USING "##########.##"; b(54);: PRINT #1, "%  ";
PRINT #1, USING "#########.##"; b(55);: PRINT #1, "%  ";
PRINT #1, USING "#########.##"; b(56);: PRINT #1, "%  ";
PRINT #1, USING "##########.##"; b(57);: PRINT #1, "%  ";
PRINT #1, ""
b(54) = b(45) - b(52)
b(55) = b(46) - b(53)
b(56) = b(1) - b(48)
b(57) = b(54) + b(55) + b(56)
b(54) = b(54) / (b(26) + b(27) + b(0) - b(47)) * 100
b(55) = b(55) / (b(26) + b(27) + b(0) - b(47)) * 100
b(56) = b(56) / (b(26) + b(27) + b(0) - b(47)) * 100
b(57) = b(54) + b(55) + b(56)
PRINT #1, "DOUBLE BUST ";
PRINT #1, USING "##########.##"; b(54);: PRINT #1, "%  ";
PRINT #1, USING "#########.##"; b(55);: PRINT #1, "%  ";
PRINT #1, USING "#########.##"; b(56);: PRINT #1, "%  ";
PRINT #1, USING "##########.##"; b(57);: PRINT #1, "%  ";
PRINT #1, ""
b(54) = b(26)
b(55) = b(27)
b(56) = b(0) - b(47)
b(57) = b(54) + b(55) + b(56)
b(54) = b(54) / (b(26) + b(27) + b(0) - b(47)) * 100
b(55) = b(55) / (b(26) + b(27) + b(0) - b(47)) * 100
b(56) = b(56) / (b(26) + b(27) + b(0) - b(47)) * 100
b(57) = b(54) + b(55) + b(56)
PRINT #1, "TOTAL       ";
PRINT #1, USING "##########.##"; b(54);: PRINT #1, "%  ";
PRINT #1, USING "#########.##"; b(55);: PRINT #1, "%  ";
PRINT #1, USING "#########.##"; b(56);: PRINT #1, "%  ";
PRINT #1, USING "##########.##"; b(57);: PRINT #1, "%  ";
PRINT #1, ""
PRINT #1, ""
PRINT #1, ""
PRINT #1, "The following table displays information gathered during";
PRINT #1, " the simulation about"
PRINT #1, "the number of times the dealer paid the player for ";
PRINT #1, "obtaining each of a set of"
PRINT #1, "user-defined predetermined winning card combinations and ";
PRINT #1, "the bonus payoff if any."
PRINT #1, ""
PRINT #1, "Card Combination      Number             Bonus Payoff"
PRINT #1, ""
PRINT #1, ""
PRINT #1, "blackjack      ";
PRINT #1, USING "#############"; mc21po(13, 1) - mc21po(13, 2);
PRINT #1, "             ";
PRINT #1, USING "##########.#"; blackjack - 1;
PRINT #1, " : 1 Odds "
IF bjsh% = 1 THEN
    PRINT #1, "blackjack      "
    PRINT #1, "after a split  ";
    PRINT #1, USING "#############"; mc21po(12, 1) - mc21po(12, 2);
    PRINT #1, "             ";
    PRINT #1, USING "##########.#"; blackjack - 1;
    PRINT #1, " : 1 Odds "
END IF
IF mc21po(4, 0) <> 0 THEN
    PRINT #1, "6 Card 27      ";
    PRINT #1, USING "#############"; mc21po(4, 1) - mc21po(4, 2);: PRINT #1, "             ";
    PRINT #1, USING "##########.#"; mc21po(4, 0);: IF ptm(1) <> 1 THEN PRINT #1, " : 1 Odds " ELSE PRINT #1, " times table minimum"
END IF
IF mc21po(5, 0) <> 0 THEN
    PRINT #1, "7 Card 27      ";
    PRINT #1, USING "#############"; mc21po(5, 1) - mc21po(5, 2);: PRINT #1, "             ";
    PRINT #1, USING "##########.#"; mc21po(5, 0);: IF ptm(1) <> 1 THEN PRINT #1, " : 1 Odds " ELSE PRINT #1, " times table minimum"
END IF
IF mc21po(6, 0) <> 0 THEN
    PRINT #1, "8 Card 27      ";
    PRINT #1, USING "#############"; mc21po(6, 1) - mc21po(6, 2);: PRINT #1, "             ";
    PRINT #1, USING "##########.#"; mc21po(6, 0);: IF ptm(1) <> 1 THEN PRINT #1, " : 1 Odds " ELSE PRINT #1, " times table minimum"
END IF
IF mc21po(7, 0) <> 0 THEN
    PRINT #1, "9 Card 27      ";
    PRINT #1, USING "#############"; mc21po(7, 1) - mc21po(7, 2);: PRINT #1, "             ";
    PRINT #1, USING "##########.#"; mc21po(7, 0);: IF ptm(1) <> 1 THEN PRINT #1, " : 1 Odds " ELSE PRINT #1, " times table minimum"
    PRINT #1, ""
END IF
IF ms678 <> 0 THEN
    PRINT #1, "678 mixed suits";
    PRINT #1, USING "#############"; mc21po(8, 1) - mc21po(8, 2);: PRINT #1, "             ";
    PRINT #1, USING "##########.#"; ms678;: IF ptm(2) <> 1 THEN PRINT #1, " : 1 Odds " ELSE PRINT #1, " times table minimum"
END IF
IF ss678 <> 0 THEN
    PRINT #1, "678 same suit  ";
    PRINT #1, USING "#############"; mc21po(9, 1) - mc21po(9, 2);: PRINT #1, "             ";
    PRINT #1, USING "##########.#"; ss678;: IF ptm(2) <> 1 THEN PRINT #1, " : 1 Odds " ELSE PRINT #1, " times table minimum"
END IF
IF sp678 <> 0 THEN
    PRINT #1, "678 spades suit";
    PRINT #1, USING "#############"; mc21po(10, 1) - mc21po(10, 2);: PRINT #1, "             ";
    PRINT #1, USING "##########.#"; sp678;: IF ptm(2) <> 1 THEN PRINT #1, " : 1 Odds " ELSE PRINT #1, " times table minimum"
    PRINT #1, ""
END IF
IF ms777 <> 0 THEN
    PRINT #1, "777 mixed suits";
    PRINT #1, USING "#############"; mc21po(1, 1) - mc21po(1, 2);: PRINT #1, "             ";
    PRINT #1, USING "##########.#"; ms777;: IF ptm(3) <> 1 THEN PRINT #1, " : 1 Odds " ELSE PRINT #1, " times table minimum"
END IF
IF ss777 <> 0 THEN
    PRINT #1, "777 same suit  ";
    PRINT #1, USING "#############"; mc21po(2, 1) - mc21po(2, 2);: PRINT #1, "             ";
    PRINT #1, USING "##########.#"; ss777;: IF ptm(3) <> 1 THEN PRINT #1, " : 1 Odds " ELSE PRINT #1, " times table minimum"
END IF
IF sp777 <> 0 THEN
    PRINT #1, "777 spades suit";
    PRINT #1, USING "#############"; mc21po(3, 1) - mc21po(3, 2);: PRINT #1, "             ";
    PRINT #1, USING "##########.#"; sp777;: IF ptm(3) <> 1 THEN PRINT #1, " : 1 Odds " ELSE PRINT #1, " times table minimum"
END IF
IF jp777 <> 0 THEN
    PRINT #1, "ss777+dealer 7 ";
    PRINT #1, USING "#############"; mc21po(11, 1) - mc21po(11, 2);: PRINT #1, "             ";
    PRINT #1, USING "##########.#"; jp777;: IF ptm(3) <> 1 THEN PRINT #1, " : 1 Odds " ELSE PRINT #1, " times table minimum"
    PRINT #1, ""
END IF
PRINT #1, ""
PRINT #1, ""
PRINT #1, ""
PRINT #1, ""
CLOSE #1
id(b(16)) = b(36) - purse: idr(b(16)) = b(15): ide(b(16)) = b(2) - purse: idf(b(16)) = b(18): ld(b(16)) = b(17)
id(0) = id(0) + id(b(16)): idr(0) = idr(0) + idr(b(16)): ide(0) = ide(0) + ide(b(16)): idf(0) = idf(0) + idf(b(16)): ld(0) = ld(0) + ld(b(16))
b(39) = b(39) + b(22): b(40) = b(40) + b(24): b(41) = b(41) + b(26): b(42) = b(42) + b(27)
b(37) = purse: b(36) = purse: b(2) = purse: b(29) = purse: b(22) = 0: b(24) = 0: b(26) = 0: b(27) = 0: b(15) = 0: b(18) = 0: b(17) = 0
b(47) = b(0): b(48) = b(1): b(49) = b(3): b(50) = b(19): b(51) = b(20): b(52) = b(45): b(53) = b(46)
FOR x = 1 TO 13
    mc21po(x, 2) = mc21po(x, 1)
NEXT x
757 b(23) = 0: b(25) = 0: dh(1) = 0
surrender% = 0: cv(13) = 0
IF card(13) < shf * deck% * mc(13) / 100 THEN b(14) = b(14) + 1: GOTO 707
GOTO 323
543 CLS:
PRINT "ULTIMA283.BAS asks the user to enter the folder/filename prefix"
PRINT "to be used to store application output. ULTIMA283.BAS asks the user"
PRINT "to enter the folder/filename prefix of a set of files containing"
PRINT "the user-defined set of game rules and matching-strategy tables for"
PRINT "a blackjack game. ULTIMA282.BAS, BJ282.BAS, and BJ293.BAS are each"
PRINT "capable of saving such a set of files. ULTIMA283.BAS uses the data"
PRINT "contained in the set of files to program a computer-controlled player"
PRINT "to use the matching-strategy tables to make decisions on how to play"
PRINT "the player's hand; and to specify a set of game rules for a"
PRINT "high-speed simulation of the game of blackjack. ULTIMA283.BAS enables"
PRINT "the user to specify a betting strategy, and program the computer-"
PRINT "controlled player to use the betting strategy to make decisions on"
PRINT "how to bet.  ULTIMA283.BAS also enables the user to specify a card-"
PRINT "counting strategy, and program the computer-controlled player to"
PRINT "use the card counting strategy to make decisions on how to count"
PRINT "cards. ULTIMA283.BAS also enables the user to use simulator results"
PRINT "to converge upon the card counting strategy that work best with"
PRINT "the user-defined betting strategy. ULTIMA283.BAS tracks various"
PRINT "statistics and saves these to a set of text files."
PRINT ""
b$ = "A23456789TJQK"
o$ = "     12/A612/A712/A812/A913/A613/A713/A813/A914/A614/A714/A814/A915/A615/A715/A815/A916/A616/A716/A816/A917/A617/A717/A817/A918/A718/A818/A919/A819/A9SplitSrndrDDown"
69 PRINT "enter folder/filename prefix to be used to store"
PRINT "application output": INPUT files$
PRINT " "
PRINT "enter the folder/filename prefix of a set of files containing data"
PRINT "about the user-defined set of game rules and matching-strategy tables"
PRINT "for a game of blackjack": INPUT D$
PRINT " "
v$ = D$ + "a.dat"
OPEN v$ FOR RANDOM AS #1 LEN = LEN(sd)
GET #1, , sd
card(0) = sd.c1: card(1) = sd.c2: card(2) = sd.c3: card(3) = sd.c4: card(4) = sd.c5
card(5) = sd.c6: card(6) = sd.c7: card(7) = sd.c8: card(8) = sd.c9: card(9) = sd.cT
card(10) = sd.cJ: card(11) = sd.cQ: card(12) = sd.cK: dwt% = sd.dwt: dbc% = sd.dbc
j = sd.j: hole% = sd.hole: schw% = sd.schw: blackjack = sd.blackjack: ins% = sd.ins
splt% = sd.splt: most% = sd.nsh: rsa% = sd.rsa: das% = sd.das: dh% = sd.dh
ld% = sd.ld: ls% = sd.ls: early% = sd.early: bjsh% = sd.bjsh: dsa% = sd.dsa
push% = sd.push: ten% = sd.ten: deck% = sd.deck: shf = sd.shf: immed% = sd.immed: pbd% = sd.pbd
b21po% = sd.b21po: ms777 = sd.ms777: ss777 = sd.ss777: sp777 = sd.sp777: jp777 = sd.jp777
mc21po(4, 0) = sd.po5c21: mc21po(5, 0) = sd.po6c21: mc21po(6, 0) = sd.po7c21: mc21po(7, 0) = sd.po8c21
ms678 = sd.ms678: ss678 = sd.ss678: sp678 = sd.sp678: dd% = sd.dd
dd(3) = sd.dd3: dd(4) = sd.dd4: dd(5) = sd.dd5: dd(6) = sd.dd6: dd(7) = sd.dd7
t$ = sd.description
CLOSE #1
FOR x = 0 TO 12
    mc(x) = card(x)
NEXT x
mc(13) = mc(0) + mc(1) + mc(2) + mc(3) + mc(4) + mc(5) + mc(6) + mc(7) + mc(8) + mc(9) + mc(10) + mc(11) + mc(12)
v$ = D$ + "b.dat"
OPEN v$ FOR RANDOM AS #1 LEN = LEN(num)
w = 0
FOR x = 0 TO 54
    FOR y = 0 TO 9
        w = w + 1
        GET #1, w, num
        r(x, y) = num.r: far(x, y) = num.far: sar(x, y) = num.sar: Hit(x, y) = num.hit
NEXT y: NEXT x
CLOSE #1
PRINT "basic strategy loaded is described as follows: ": PRINT t$
PRINT "if this is acceptable type 'y' and press enter ";: INPUT ans$: IF ans$ <> "y" THEN GOTO 69
v$ = files$ + "i.txt"
OPEN v$ FOR OUTPUT AS #1
PRINT #1, "enter folder/filename prefix to be used to store"
PRINT #1, "application output": PRINT #1, files$
PRINT #1, " "
PRINT #1, "enter the folder/filename prefix of a set of files containing data"
PRINT #1, "about the user-defined set of game rules and matching-strategy tables"
PRINT #1, "for a game of blackjack": PRINT #1, D$
PRINT #1, " "
PRINT #1, "strategy loaded is described as follows: "; t$
PRINT #1, "if this is acceptable type 'y' and press enter ": PRINT #1, ans$
CLOSE #1
IF rsa% = 1 THEN rsa% = most% ELSE rsa% = 2
ERASE cv
79 PRINT "press 'y' and enter to load an ezcount strategy ";: INPUT ans$
OPEN v$ FOR APPEND AS #1
PRINT #1, "press 'y' and enter to load an ezcount strategy ";: PRINT #1, ans$
CLOSE #1
IF ans$ = "y" THEN
    PRINT "enter ‘folder/filename' prefix of ezcount strategy to be loaded": INPUT z$
    zv$ = z$ + "z.dat"
    OPEN zv$ FOR RANDOM AS #1 LEN = LEN(ez)
    GET #1, , ez
    dv(0) = ez.dv1: dv(1) = ez.dv2: dv(2) = ez.dv3: dv(3) = ez.dv4: dv(4) = ez.dv5
    dv(5) = ez.dv6: dv(6) = ez.dv7: dv(7) = ez.dv8: dv(8) = ez.dv9: dv(9) = ez.dv10
    dv(10) = ez.dv11: dv(11) = ez.dv12: dv(12) = ez.dv13
    fv(0) = ez.fv1: fv(1) = ez.fv2: fv(2) = ez.fv3: fv(3) = ez.fv4: fv(4) = ez.fv5
    fv(5) = ez.fv6: fv(6) = ez.fv7: fv(7) = ez.fv8: fv(8) = ez.fv9: fv(9) = ez.fv10
    fv(10) = ez.fv11: fv(11) = ez.fv12: fv(12) = ez.fv13
    k$ = ez.description
    PRINT "ezcount loaded is described as "; k$
    I = 1
    CLOSE #1
END IF
IF ans$ = "y" THEN PRINT "if this is acceptable press 'y' and enter";: INPUT ans$: IF ans$ <> "y" THEN GOTO 79
OPEN v$ FOR APPEND AS #1
IF ans$ = "y" THEN
    PRINT #1, "enter folder/filename prefix of ezcount strategy to be loaded ": PRINT #1, z$
    PRINT #1, "ezcount loaded is described as "; k$
    PRINT #1, "if this is acceptable press 'y' and enter ";: PRINT #1, ans$
END IF
CLOSE #1
IF ans$ <> "y" THEN
    FOR x = 0 TO 12
    READ a: dv(x) = a: fv(x) = 100: NEXT x
    DATA -1,1,1,1,1,1,0,0,0,-1,-1,-1,-1
    PRINT "press 1 and enter for default count values ";: INPUT I
END IF
FOR x = 0 TO 12
    IF I = 1 THEN c = dv(x) / fv(x) * 100: GOTO 808
    PRINT "what is the count value of ";: PRINT MID$(b$, x + 1, 1);: PRINT "'s ("; dv(x); ")";: INPUT c: dv(x) = c
808 cv(x) = c: cv(13) = cv(13) - cv(x) * mc(x) * deck%: ev(13) = ev(13) + cv(x) * mc(x) / 4: gv(14) = gv(14) + ABS(cv(x)) * mc(x) / 4: NEXT x
OPEN v$ FOR APPEND AS #1
IF ans$ <> "y" THEN
    PRINT #1, "press 1 and enter for default count values "; I
    FOR x = 0 TO 12
        PRINT #1, "what is the count value of ";: PRINT #1, MID$(b$, x + 1, 1);: PRINT #1, "'s ("; dv(x); ")":
    NEXT x
END IF
CLOSE #1
809 PRINT "press 1 and enter for true count conversion ";: INPUT true%
PRINT "input a constant added to render the bet count ";: INPUT constant
PRINT " "
PRINT "press 1 and enter to use ongoing simulation results"
PRINT "to generate an ezcount strategy ";: INPUT ezc%
PRINT " "
PRINT "Press 1 and enter to mathematically balance"
PRINT "the card counting strategy if unbalanced ";: INPUT bal%
PRINT " "
IF bal% = 1 THEN
    PRINT "Press 1 and enter or 2 and enter to specify the type of balancing to be used. "
    PRINT "1 for subtractive, 2 for multiplicative": INPUT balance%
END IF
IF balance% = 1 THEN
    FOR x = 0 TO 12
        gv(x) = cv(x) - ev(13) * mc(x) / mc(13)
        cv(x) = gv(x)
    NEXT x
    cv(13) = 0
END IF
IF balance% = 2 THEN
    FOR x = 0 TO 12
        IF cv(x) >= 0 THEN gv(x) = cv(x) * gv(14) / (gv(14) + ev(13))
        IF cv(x) < 0 THEN gv(x) = cv(x) * gv(14) / (gv(14) - ev(13))
        cv(x) = gv(x)
    NEXT x
    cv(13) = 0: gv(13) = 0
END IF
IF ezc% = 1 AND bal% = 1 THEN
    cv(13) = 0
    FOR x = 0 TO 12
        cv(13) = cv(13) - cv(x) * mc(x) * deck%
    NEXT x
    PRINT " "
    PRINT "press 1 and enter to generate the ezcount strategy by only using"
    PRINT "ongoing simulation results gathered from the outcome of games started"
    PRINT "within a user-defined range of bet counts ";: INPUT x
    PRINT " "
    IF x = 1 THEN
        PRINT "input the upper limit to the bet count used to generate ezcount strategy ";: INPUT top!
        PRINT "input the lower limit to the bet count used to generate ezcount strategy ";: INPUT bottom!
    END IF
END IF
OPEN v$ FOR APPEND AS #1
PRINT #1, "press 1 and enter for true count conversion ";: PRINT #1, true%
PRINT #1, "input a constant added to render the bet count ";: PRINT #1, constant
PRINT #1, "press 1 and enter to use ongoing simulation results to generate an ezcount strategy ";: PRINT #1, ezc%
PRINT #1, "Press 1 and enter to mathematically balance count strategy if unbalanced ";: PRINT #1, bal%
IF bal% = 1 THEN
    PRINT #1, "Press 1 and enter, or 2 and enter to specify the type of balancing to be used: "
    PRINT #1, "1 for subtractive, 2 for multiplicative ": PRINT #1, balance%
END IF
top! = 10000: bottom! = -10000
IF ezc% = 1 AND bal% = 1 THEN
    PRINT #1, " "
    PRINT #1, "press 1 and enter to generate the ezcount strategy by only using"
    PRINT #1, "ongoing simulation results gathered from the outcome of games started"
    PRINT #1, "within a user-defined range of bet counts ";: PRINT #1, x
    PRINT #1, " "
    IF x = 1 THEN
        PRINT #1, "input the upper limit to the bet count used to generate ezcount strategy ";: PRINT #1, top!
        PRINT #1, "input the lower limit to the bet count used to generate ezcount strategy ";: PRINT #1, bottom!
    END IF
END IF
CLOSE #1
PRINT " "
PRINT shf;: PRINT "% is the maximum percentage of the stack of cards"
PRINT "remaining in the shoe when the dealer reshuffles."
PRINT " "
PRINT "if this is acceptable type 'y'and press enter ";: INPUT ans$:
IF ans$ <> "y" THEN
    PRINT " "
    PRINT "input the maximum percentage of the stack of cards"
    PRINT "remaining in the shoe when the dealer reshuffles (15-99)";: INPUT shf
END IF
PRINT " "
PRINT "press 1 and enter for if the bet count falls below a user-defined threshold"
PRINT "then the computer-controlled player quits the table ";: INPUT y
PRINT " "
IF y = 1 THEN
    PRINT "at what bet count does the player quit the table";: INPUT quit
    PRINT " "
    PRINT "press 1 and enter to multiply the user-defined threshold "
    PRINT "by the percent of the stack remaining in the shoe ";: INPUT truequit%
    PRINT " "
END IF
IF y <> 1 THEN quit = -10000
PRINT "press 1 and enter for if the bet count rises above a user-defined threshold"
PRINT "then the dealer reshuffles ";: INPUT z
PRINT " "
IF z = 1 THEN PRINT "at what bet count does the dealer reshuffle";: INPUT reshf: PRINT " "
IF z <> 1 THEN reshf = 10000
PRINT "input as a number of credits the table minimum";: INPUT mini&
PRINT "input as a number of credits the table maximum";: INPUT maxi&
PRINT "input as a number of credits the fee paid for every 100 credits bet";: INPUT ante%
PRINT "input as a number of credits the starting purse";: INPUT purse: b(37) = purse: b(36) = purse: b(2) = purse: b(29) = purse
PRINT "compute bets in multiples of what number of credits";: INPUT mult%
PRINT "press 1 and enter for a multiplicative betting system";: INPUT multi%
IF multi% = 1 THEN PRINT "bet = multiplier * bet count, input the multiplier ";: INPUT factor
IF multi% <> 1 THEN PRINT "press 1 and enter for an exponential betting system";: INPUT expo%
IF expo% = 1 THEN PRINT "bet = multiplier * base ^ bet count, input the base ";: INPUT factor
IF expo% = 1 THEN PRINT "bet = multiplier * base ^ bet count, input the multiplier ";: INPUT multiplier
PRINT "press 1 and enter to limit bets by percentage of purse";: INPUT limper%
IF limper% <> 1 THEN hibet = 100 ELSE PRINT "as a % of purse input the size of the smallest bet";: INPUT lobet: PRINT "as a % of purse input the size of the biggest bet";: INPUT hibet
PRINT "press 1 and enter to limit bets to a range of values";: INPUT limpoi%
IF limpoi% <> 1 THEN lrg& = 1E+09 ELSE PRINT "as a number of credits what is the smallest player bet";: INPUT sml&: PRINT "as a number of point what is the largest player bet";: INPUT lrg&
PRINT "press 1 and enter for a plateau betting system";: INPUT pbs%
IF pbs% = 1 THEN PRINT "bet = bet * record high purse / original purse": pbs = purse
IF pbs% <> 1 THEN PRINT "press 1 and enter for a proportional betting system";: INPUT propor%
IF propor% = 1 THEN PRINT "bet= bet * current purse / original purse": pbs = purse
IF ins% = 1 THEN
PRINT " "
PRINT "specify one of three insurance-side-bet ";
PRINT "strategies."
PRINT " "
PRINT "Press enter for the player ";
PRINT "never makes an insurance-side bet."
PRINT "Press 1 and enter for the player ";
PRINT "makes an insurance-side bet"
PRINT "whenever the bet count indicates ";
PRINT "doing so would be profitable."
PRINT "Press 2 and enter for the player ";
PRINT "always makes an insurance-side bet."
INPUT insmode%
PRINT " "
END IF
IF b21po% = 1 THEN
    IF mc21po(4, 0) + mc21po(5, 0) + mc21po(6, 0) + mc21po(7, 0) <> 0 THEN
        PRINT " "
        PRINT "Press 1 and enter for dealer makes bonus payouts a multiple of the table minimum"
        PRINT "on five, six, seven, and or eight-card hands valued at twenty-one points"
        INPUT ptbm%: ptm(1) = ptbm%
    END IF
    IF ms678 + ss678 + sp678 <> 0 THEN
        PRINT "Press 1 and enter for dealer makes bonus payouts a multiple of the table minimum"
        PRINT "on 6-7-8 hands "
        INPUT ptbm%: ptm(2) = ptbm%
    END IF
    IF ms777 + ss777 + sp777 + jp777 <> 0 THEN
        PRINT "Press 1 and enter for dealer makes bonus payouts a multiple of the table minimum"
        PRINT "on 7-7-7 hands "
        INPUT ptbm%: ptm(3) = ptbm%
    END IF
END IF
PRINT "what is the number of additional players at the table (0-6) ";: INPUT ap
PRINT "input the number of sessions in this simulation (1 - 10000) ";: INPUT hmr&
PRINT "input the number of rounds of play in each session (1 -"; FIX(2100000000 / hmr&); ") ";: INPUT scr&
hmr& = hmr& * scr&
CLS
OPEN v$ FOR APPEND AS #1
PRINT #1, " "
PRINT #1, shf;: PRINT #1, "% is the maximum percentage of the stack remaining in the shoe"
PRINT #1, "when the dealer reshuffles."
PRINT #1, " "
PRINT #1, "if this is acceptable type 'y'and press enter ";: PRINT #1, ans$
IF ans$ <> "y" THEN
    PRINT #1, " "
    PRINT #1, "input the maximum percentage of the stack remaining in the shoe"
    PRINT #1, "when the dealer reshuffles (15-99)";: PRINT #1, shf
END IF
PRINT #1, " "
PRINT #1, "press 1 and enter for if the bet count falls below a user-defined threshold"
PRINT #1, "then the computer-controlled player quits the table ";: PRINT #1, y
PRINT #1, " "
IF y = 1 THEN
    PRINT #1, "at what bet count does the player quit the table ";: PRINT #1, quit
    PRINT #1, " "
    PRINT #1, "press 1 and enter to multiply the user-defined threshold "
    PRINT #1, "by the percent of the stack remaining in the shoe ";: PRINT #1, truequit%
    PRINT #1, " "
END IF
PRINT #1, "press 1 and enter for if the bet count rises above a user-defined threshold"
PRINT #1, "then the dealer reshuffles";: PRINT #1, z
PRINT #1, " "
IF z = 1 THEN PRINT #1, "at what bet count does the dealer reshuffle ";: PRINT #1, reshf: PRINT #1, " "
PRINT #1, "input as a number of credits the table minimum ";: PRINT #1, mini&
PRINT #1, "input as a number of credits the table maximum ";: PRINT #1, maxi&
PRINT #1, "input as a number of credits the fee paid for every 100 credits bet ";: PRINT #1, ante%
PRINT #1, "input as a number of credits the starting purse ";: PRINT #1, purse
PRINT #1, "compute bets in multiples of what number of credits ";: PRINT #1, mult%
PRINT #1, "press 1 and enter for a multiplicative betting system ";: PRINT #1, multi%
IF multi% = 1 THEN PRINT #1, "bet = multiplier * bet count, input the multiplier ";: PRINT #1, factor
PRINT #1, "press 1 and enter for an exponential betting system ";: PRINT #1, expo%
IF expo% = 1 THEN PRINT #1, "bet = multiplier * base ^ bet count, input the base ";: PRINT #1, factor
IF expo% = 1 THEN PRINT #1, "bet = multiplier * base ^ bet count, input the multiplier ";: PRINT #1, multiplier
PRINT #1, "press 1 and enter to limit bet size by percentage of purse ";: PRINT #1, limper%
IF limper% <> 1 THEN hibet = 100 ELSE PRINT #1, "as a % of purse input the size of the smallest bet ";: PRINT #1, lobet: PRINT #1, "as a % of purse input the size of the biggest bet ";: PRINT #1, hibet
PRINT #1, "press 1 and enter to limit bets by number of credits";: PRINT #1, limpoi%
IF limpoi% <> 1 THEN lrg& = 1E+09 ELSE PRINT #1, "as a number of credits what is the smallest player bet ";: PRINT #1, sml&: PRINT #1, "as a number of point what is the largest player bet ";: PRINT #1, lrg&
PRINT #1, "press 1 and enter for a plateau betting system ";: PRINT #1, pbs%
IF pbs% = 1 THEN PRINT #1, "bet = bet * record high purse / original purse "
IF pbs% <> 1 THEN PRINT #1, "press 1 and enter for a proportional betting system ";: PRINT #1, propor%
IF propor% = 1 THEN PRINT #1, "bet = bet * current purse / original purse "
IF ins% = 1 THEN
PRINT #1, " "
PRINT #1, "specify one of three insurance-side-bet ";
PRINT #1, "strategies."
PRINT #1, " "
PRINT #1, "press enter for the player ";
PRINT #1, "never makes an insurance-side bet."
PRINT #1, "press 1 and enter for the player ";
PRINT #1, "makes an insurance-side bet"
PRINT #1, "whenever the bet count indicates ";
PRINT #1, "doing so would be profitable."
PRINT #1, "press 2 and enter for the player ";
PRINT #1, "always makes an insurance-side bet."
PRINT #1, insmode%
PRINT #1, " "
END IF
IF b21po% = 1 THEN
    IF mc21po(4, 0) + mc21po(5, 0) + mc21po(6, 0) + mc21po(7, 0) <> 0 THEN
        PRINT #1, "Press 1 and enter for dealer makes bonus payouts a multiple of the table minimum"
        PRINT #1, "on five, six, seven, and or eight-card hands valued at twenty-one points"
        PRINT #1, ptm(1)
    END IF
    IF ms678 + ss678 + sp678 <> 0 THEN
        PRINT #1, "Press 1 and enter for dealer makes bonus payouts a multiple of the table minimum"
        PRINT #1, "on 6-7-8 hands "
        PRINT #1, ptm(2)
    END IF
    IF ms777 + ss777 + sp777 + jp777 <> 0 THEN
        PRINT #1, "Press 1 and enter for dealer makes bonus payouts a multiple of the table minimum"
        PRINT #1, "on 7-7-7 hands "
        PRINT #1, ptm(3)
    END IF
END IF
PRINT #1, "input the number of additional players at the table (0-6)";: PRINT #1, ap
PRINT #1, "input the number of sessions in this simulation ";: PRINT #1, hmr& / scr&
PRINT #1, "input the number of rounds of play in each session ";: PRINT #1, scr&
PRINT #1, ""
PRINT #1, ""
PRINT #1, "START: "; DATE$; " "; TIME$
CLOSE #1
v$ = files$ + "o.txt"
OPEN v$ FOR OUTPUT AS #1
PRINT #1, ""
CLOSE #1
RANDOMIZE TIMER
REM initalizing stack of cards
z = 0: suitp(3) = 104: suitp(4) = 100: suitp(5) = 99: suitp(6) = 115
pdenom(0) = 65: pdenom(1) = 50: pdenom(2) = 51: pdenom(3) = 52: pdenom(4) = 53: pdenom(5) = 54: pdenom(6) = 55
pdenom(7) = 56: pdenom(8) = 57: pdenom(9) = 84: pdenom(10) = 74: pdenom(11) = 81: pdenom(12) = 75
FOR w = 1 TO deck%
    FOR x = 0 TO 12
        FOR y = 0 TO mc(x) - 1
            denom(z) = x: suit(z) = y MOD 4 + 3
            IF x < 9 THEN av(z) = x ELSE av(z) = 9
            stack(z) = z MOD mc(13)
            z = z + 1
NEXT y: NEXT x: NEXT w
z = 0: w = 0
FOR x = 12 TO 19
    FOR y = 5 TO 8
        IF x > y + 12 THEN y = x - 12
        z = z + 1
        g(z) = x: H(z) = y
NEXT y: NEXT x: g(30) = 1000: g(31) = 1000: g(0) = 1000: H(30) = 1000: H(31) = 1000: H(0) = 1000
GOTO 707
STOP
999 IF ezc% = 1 AND bal% = 1 THEN
    v$ = files$ + "z.dat"
    OPEN v$ FOR RANDOM AS #1 LEN = LEN(ez)
    ez.dv1 = dv(0): ez.dv2 = dv(1): ez.dv3 = dv(2): ez.dv4 = dv(3): ez.dv5 = dv(4)
    ez.dv6 = dv(5): ez.dv7 = dv(6): ez.dv8 = dv(7): ez.dv9 = dv(8): ez.dv10 = dv(9)
    ez.dv11 = dv(10): ez.dv12 = dv(11): ez.dv13 = dv(12)
    ez.fv1 = fv(0): ez.fv2 = fv(1): ez.fv3 = fv(2): ez.fv4 = fv(3): ez.fv5 = fv(4)
    ez.fv6 = fv(5): ez.fv7 = fv(6): ez.fv8 = fv(7): ez.fv9 = fv(8): ez.fv10 = fv(9)
    ez.fv11 = fv(10): ez.fv12 = fv(11): ez.fv13 = fv(12)
    ez.description = s$
    PUT #1, , ez
    CLOSE #1
END IF
v$ = files$ + "i.txt"
OPEN v$ FOR APPEND AS #1
PRINT #1, ""
PRINT #1, "END:   "; DATE$; " "; TIME$
CLOSE #1
v$ = files$ + "d.txt"
OPEN v$ FOR OUTPUT AS #1
PRINT #1, "ULTIMA283.BAS PROGRAM OUTPUT"
PRINT #1, " "
PRINT #1, "number of rounds played during each session is           : ";: PRINT #1, USING "##########"; scr&
PRINT #1, "number of sessions completed during entire simulation is : ";: PRINT #1, USING "##########"; b(16)
PRINT #1, ""
PRINT #1, "average size of the initial drop is                      : ";: PRINT #1, USING "##########"; id(0) / b(16);: PRINT #1, " credits"
PRINT #1, "average duration of drop from start to lowest point is   : ";: PRINT #1, USING "##########"; idr(0) / b(16);: PRINT #1, " rounds"
PRINT #1, "average duration of drop from start to finish is         : ";: PRINT #1, USING "##########"; idf(0) / b(16);: PRINT #1, " rounds"
PRINT #1, "average size of the largest drop from a new high is      : ";: PRINT #1, USING "##########"; ld(0) / b(16);: PRINT #1, " credits"
PRINT #1, "average bankroll change at the end of session is         : ";: PRINT #1, USING "##########"; ide(0) / b(16);: PRINT #1, " credits"
PRINT #1, ""
PRINT #1, "total number of wagers won by house  :    ";: PRINT #1, USING "##########.#"; b(42) / 2: PRINT #1, "total number of wagers won by player :    ";: PRINT #1, USING "##########.#"; b(41) / 2
PRINT #1, "player is winning ";: PRINT #1, USING "###.##"; b(41) / (b(41) + b(42) + 1E-08) * 100;: PRINT #1, "% of all wagers won       ROUND NUMBER "; rn&& - 1; ""
PRINT #1, "total number of credits won by house :";: PRINT #1, USING "##############.#"; b(40) / 2: PRINT #1, "total number of credits won by player:";: PRINT #1, USING "##############.#"; b(39) / 2
PRINT #1, "player is winning ";: PRINT #1, USING "###.##"; b(39) / (b(39) + b(40) + 1E-08) * 100;: PRINT #1, "% of all credits won        reshuffles "; b(14); ""
PRINT #1, "finish purse ";: PRINT #1, USING "########.#"; purse + (b(39) - b(40)) / 2;: PRINT #1, " high ";: PRINT #1, USING "########.#"; b(44);
PRINT #1, " low ";: PRINT #1, USING "########.#"; b(43);: PRINT #1, "  largest drop ";: PRINT #1, USING "########.#"; drop
PRINT #1, "change in purse is an average of ";: PRINT #1, USING "##########.##"; b(31);: PRINT #1, " credits after "; scr&; " rounds"
PRINT #1, "one standard deviation equal to  ";: PRINT #1, USING "##########.##"; SQR(b(33) / INT(rn&& / scr&));: PRINT #1, " credits"
IF ezc% <> 1 THEN
    PRINT #1, "values added to the running count as each ";
    PRINT #1, "card of a given denomination was played"
    PRINT #1, "and then removed from the stack during ";
    PRINT #1, "this game simulation were as follows:"
    PRINT #1, ""
    FOR x = 0 TO 12
        PRINT #1, MID$(b$, x + 1, 1);: PRINT #1, "     = ";: PRINT #1, USING "####.###"; cv(x);
        IF bal% = 1 THEN PRINT #1, "          Balanced ";: PRINT #1, MID$(b$, x + 1, 1);: PRINT #1, "     = ";: PRINT #1, USING "####.###"; gv(x)
        IF bal% <> 1 THEN PRINT #1, ""
    NEXT x
    PRINT #1, ""
    PRINT #1, "TOTAL = ";: PRINT #1, USING "####.###"; ev(13);
    IF bal% = 1 THEN PRINT #1, "          Balanced TOTAL = ";: PRINT #1, USING "####.###"; 0
    PRINT #1, ""
END IF
IF ezc% = 1 THEN
    PRINT #1, "Change in expected value of the game wager caused"
    PRINT #1, "by the removal of one card of a given denomination "
    PRINT #1, "from the stack during this game simulation."
    PRINT #1, ""
    FOR x = 0 TO 12
        PRINT #1, MID$(b$, x + 1, 1);: PRINT #1, "     = ";: PRINT #1, USING "####.###"; cv(x);
        IF bal% = 1 THEN PRINT #1, "          Balanced ";: PRINT #1, MID$(b$, x + 1, 1);: PRINT #1, "     = ";: PRINT #1, USING "####.###"; gv(x)
        IF bal% <> 1 THEN PRINT #1, ""
    NEXT x
    PRINT #1, ""
    PRINT #1, "TOTAL = ";: PRINT #1, USING "####.###"; ev(13);
    IF bal% = 1 THEN PRINT #1, "          Balanced TOTAL = ";: PRINT #1, USING "####.###"; 0
    IF bal <> 1 THEN PRINT #1, ""
END IF
PRINT #1, ""
PRINT #1, "The user defined blackjack game rules for this simulation are as follows:"
PRINT #1, ""
PRINT #1, t$
PRINT #1, ""
FOR w = 1 TO b(16)
    high# = -1000000000
    FOR x = 1 TO b(16)
        IF id(x) > high# THEN
            IF order1(x) = 0 THEN
                high# = id(x)
                rank1(w) = x
            END IF
        END IF
        IF id(x) = high# THEN
            IF order1(x) = 0 THEN
                high# = id(x)
                rank1(w) = x
            END IF
        END IF
NEXT x: order1(rank1(w)) = 1: NEXT w
PRINT #1, ""
PRINT #1, ""
PRINT #1, "in the following table, each of ";: PRINT #1, USING "####"; b(16);: PRINT #1, " sessions completed during the simulation are ranked"
PRINT #1, "from smallest to biggest according to the size of the initial drop in the bankroll measured as a number of credits"
PRINT #1,
FOR x = 1 TO b(16)
    IF x = FIX(x / 100) * 100 + 1 THEN
        PRINT #1, ""
        PRINT #1, "                initial drop        start to lowest point        start to finish             largest drop           session end"
        PRINT #1, "RANK                 SIZE                 DURATION                 DURATION                      SIZE            BANKROLL CHANGE"
        PRINT #1, ""
    END IF
    PRINT #1, USING "####"; x;: PRINT #1, "        ";
    PRINT #1, USING "###########.#"; id(rank1(x));: PRINT #1, "             ";
    PRINT #1, USING "############"; idr(rank1(x));: PRINT #1, "             ";
    PRINT #1, USING "############"; idf(rank1(x));: PRINT #1, "             ";
    PRINT #1, USING "###########.#"; ld(rank1(x));: PRINT #1, "             ";
    PRINT #1, USING "###########.#"; ide(rank1(x))
NEXT x
ERASE rank1: ERASE order1
FOR w = 1 TO b(16)
    idrn& = 1000000000
    FOR x = 1 TO b(16)
        IF idr(x) < idrn& THEN
            IF order1(x) = 0 THEN
                idrn& = idr(x)
                rank1(w) = x
            END IF
        END IF
        IF idr(x) = idrn& THEN
            IF order1(x) = 0 THEN
                idrn& = idr(x)
                rank1(w) = x
            END IF
        END IF
NEXT x: order1(rank1(w)) = 1: NEXT w
PRINT #1, ""
PRINT #1, ""
PRINT #1, "in the following table, each of ";: PRINT #1, USING "####"; b(16);: PRINT #1, " sessions completed during the entire simulation are ranked"
PRINT #1, "from shortest to longest according to the duration of the initial drop from start to the lowest point measured as a number of rounds."
PRINT #1,
FOR x = 1 TO b(16)
    IF x = FIX(x / 100) * 100 + 1 THEN
        PRINT #1, ""
        PRINT #1, "                initial drop        start to lowest point        start to finish             largest drop           session end"
        PRINT #1, "RANK                 SIZE                 DURATION                 DURATION                      SIZE            BANKROLL CHANGE"
        PRINT #1, ""
    END IF
    PRINT #1, USING "####"; x;: PRINT #1, "        ";
    PRINT #1, USING "###########.#"; id(rank1(x));: PRINT #1, "             ";
    PRINT #1, USING "############"; idr(rank1(x));: PRINT #1, "             ";
    PRINT #1, USING "############"; idf(rank1(x));: PRINT #1, "             ";
    PRINT #1, USING "###########.#"; ld(rank1(x));: PRINT #1, "             ";
    PRINT #1, USING "###########.#"; ide(rank1(x))
NEXT x
ERASE rank1: ERASE order1
FOR w = 1 TO b(16)
    idrn& = 1000000000
    FOR x = 1 TO b(16)
        IF idf(x) < idrn& THEN
            IF order1(x) = 0 THEN
                idrn& = idf(x)
                rank1(w) = x
            END IF
        END IF
        IF idf(x) = idrn& THEN
            IF order1(x) = 0 THEN
                idrn& = idf(x)
                rank1(w) = x
            END IF
        END IF
NEXT x: order1(rank1(w)) = 1: NEXT w
PRINT #1, ""
PRINT #1, ""
PRINT #1, "in the following table, each of ";: PRINT #1, USING "####"; b(16);: PRINT #1, " sessions completed during the entire simulation are ranked"
PRINT #1, "from shortest to longest according to the duration of the initial drop from start to finish measured as a number of rounds."
PRINT #1,
FOR x = 1 TO b(16)
    IF x = FIX(x / 100) * 100 + 1 THEN
        PRINT #1, ""
        PRINT #1, "                initial drop        start to lowest point        start to finish             largest drop           session end"
        PRINT #1, "RANK                 SIZE                 DURATION                 DURATION                      SIZE            BANKROLL CHANGE"
        PRINT #1, ""
    END IF
    PRINT #1, USING "####"; x;: PRINT #1, "        ";
    PRINT #1, USING "###########.#"; id(rank1(x));: PRINT #1, "             ";
    PRINT #1, USING "############"; idr(rank1(x));: PRINT #1, "             ";
    PRINT #1, USING "############"; idf(rank1(x));: PRINT #1, "             ";
    PRINT #1, USING "###########.#"; ld(rank1(x));: PRINT #1, "             ";
    PRINT #1, USING "###########.#"; ide(rank1(x))
NEXT x
ERASE rank1: ERASE order1
FOR w = 1 TO b(16)
    high# = -1000000000
    FOR x = 1 TO b(16)
        IF ld(x) > high# THEN
            IF order1(x) = 0 THEN
                high# = ld(x)
                rank1(w) = x
            END IF
        END IF
        IF ld(x) = high# THEN
            IF order1(x) = 0 THEN
                high# = ld(x)
                rank1(w) = x
            END IF
        END IF
NEXT x: order1(rank1(w)) = 1: NEXT w
PRINT #1, ""
PRINT #1, ""
PRINT #1, "in the following table, each of ";: PRINT #1, USING "####"; b(16);: PRINT #1, " sessions completed during the entire simulation are ranked"
PRINT #1, "from smallest to biggest according to the size of the largest drop from a new high measured as a number of credits"
PRINT #1,
FOR x = 1 TO b(16)
    IF x = FIX(x / 100) * 100 + 1 THEN
        PRINT #1, ""
        PRINT #1, "                initial drop        start to lowest point        start to finish             largest drop           session end"
        PRINT #1, "RANK                 SIZE                 DURATION                 DURATION                      SIZE            BANKROLL CHANGE"
        PRINT #1, ""
    END IF
    PRINT #1, USING "####"; x;: PRINT #1, "        ";
    PRINT #1, USING "###########.#"; id(rank1(x));: PRINT #1, "             ";
    PRINT #1, USING "############"; idr(rank1(x));: PRINT #1, "             ";
    PRINT #1, USING "############"; idf(rank1(x));: PRINT #1, "             ";
    PRINT #1, USING "###########.#"; ld(rank1(x));: PRINT #1, "             ";
    PRINT #1, USING "###########.#"; ide(rank1(x))
NEXT x
ERASE rank1: ERASE order1
FOR w = 1 TO b(16)
    high# = -1000000000
    FOR x = 1 TO b(16)
        IF ide(x) > high# THEN
            IF order1(x) = 0 THEN
                high# = ide(x)
                rank1(w) = x
            END IF
        END IF
        IF ide(x) = high# THEN
            IF order1(x) = 0 THEN
                high# = ide(x)
                rank1(w) = x
            END IF
        END IF
NEXT x: order1(rank1(w)) = 1: NEXT w
PRINT #1, ""
PRINT #1, ""
PRINT #1, "in the following table, each of ";: PRINT #1, USING "####"; b(16);: PRINT #1, " sessions completed during the entire simulation are ranked"
PRINT #1, "from greatest gain to greatest loss according to the end of session net change in bankroll measured as a number of credits."
PRINT #1,
FOR x = 1 TO b(16)
    IF x = FIX(x / 100) * 100 + 1 THEN
        PRINT #1, ""
        PRINT #1, "                initial drop        start to lowest point        start to finish             largest drop           session end"
        PRINT #1, "RANK                 SIZE                 DURATION                 DURATION                      SIZE            BANKROLL CHANGE"
        PRINT #1, ""
    END IF
    PRINT #1, USING "####"; x;: PRINT #1, "        ";
    PRINT #1, USING "###########.#"; id(rank1(x));: PRINT #1, "             ";
    PRINT #1, USING "############"; idr(rank1(x));: PRINT #1, "             ";
    PRINT #1, USING "############"; idf(rank1(x));: PRINT #1, "             ";
    PRINT #1, USING "###########.#"; ld(rank1(x));: PRINT #1, "             ";
    PRINT #1, USING "###########.#"; ide(rank1(x))
NEXT x
PRINT #1, ""
PRINT #1, ""
PRINT #1, ""
PRINT #1, ""
PRINT #1, "The following is a table displaying the number of game wagers resolved"
PRINT #1, "by each possible outcome wherein on a win the dealer rewards the player"
PRINT #1, "on the game wager, on a loss the dealer collects the game wager from the"
PRINT #1, "the player, and on a push the dealer returns the game wager to the player."
PRINT #1, "Each surrender is counted as loss of a half wager and push of a half wager."
PRINT #1, "Each double down is counted as either win, loss, or push of two game wagers."
PRINT #1, ""
PRINT #1, ""
PRINT #1, "                      WIN           LOSS           PUSH           TOTAL"
PRINT #1, ""
b(54) = b(41) - b(45) - b(20)
b(55) = b(42) - b(46) - b(19)
b(56) = b(0) - b(1)
b(57) = b(54) + b(55) + b(56)
PRINT #1, "NO BUST     ";
PRINT #1, USING "###########.#"; b(54) / 2;: PRINT #1, "   ";
PRINT #1, USING "##########.#"; b(55) / 2;: PRINT #1, "   ";
PRINT #1, USING "##########.#"; b(56) / 2;: PRINT #1, "   ";
PRINT #1, USING "###########.#"; b(57) / 2;: PRINT #1, "   ";
PRINT #1, ""
b(54) = 0
b(55) = b(19)
b(56) = 0
b(57) = b(54) + b(55) + b(56)
PRINT #1, "PLAYER BUST ";
PRINT #1, USING "###########.#"; b(54) / 2;: PRINT #1, "   ";
PRINT #1, USING "##########.#"; b(55) / 2;: PRINT #1, "   ";
PRINT #1, USING "##########.#"; b(56) / 2;: PRINT #1, "   ";
PRINT #1, USING "###########.#"; b(57) / 2;: PRINT #1, "   ";
PRINT #1, ""
b(54) = b(20)
b(55) = 0
b(56) = 0
b(57) = b(54) + b(55) + b(56)
PRINT #1, "DEALER BUST ";
PRINT #1, USING "###########.#"; b(54) / 2;: PRINT #1, "   ";
PRINT #1, USING "##########.#"; b(55) / 2;: PRINT #1, "   ";
PRINT #1, USING "##########.#"; b(56) / 2;: PRINT #1, "   ";
PRINT #1, USING "###########.#"; b(57) / 2;: PRINT #1, "   ";
PRINT #1, ""
b(54) = b(45)
b(55) = b(46)
b(56) = b(1)
b(57) = b(54) + b(55) + b(56)
PRINT #1, "DOUBLE BUST ";
PRINT #1, USING "###########.#"; b(54) / 2;: PRINT #1, "   ";
PRINT #1, USING "##########.#"; b(55) / 2;: PRINT #1, "   ";
PRINT #1, USING "##########.#"; b(56) / 2;: PRINT #1, "   ";
PRINT #1, USING "###########.#"; b(57) / 2;: PRINT #1, "   ";
PRINT #1, ""
b(54) = b(41)
b(55) = b(42)
b(56) = b(0)
b(57) = b(54) + b(55) + b(56)
PRINT #1, "TOTAL       ";
PRINT #1, USING "###########.#"; b(54) / 2;: PRINT #1, "   ";
PRINT #1, USING "##########.#"; b(55) / 2;: PRINT #1, "   ";
PRINT #1, USING "##########.#"; b(56) / 2;: PRINT #1, "   ";
PRINT #1, USING "###########.#"; b(57) / 2;: PRINT #1, "   ";
PRINT #1, ""
PRINT #1, ""
PRINT #1, ""
PRINT #1, "The following is a table displaying percentage of game wagers resolved"
PRINT #1, "by each possible outcome"
PRINT #1, ""
PRINT #1, ""
PRINT #1, "                      WIN           LOSS           PUSH           TOTAL"
PRINT #1, ""
b(54) = b(41) - b(45) - b(20)
b(55) = b(42) - b(46) - b(19)
b(56) = b(0) - b(1)
b(57) = b(54) + b(55) + b(56)
b(54) = b(54) / (b(41) + b(42) + b(0)) * 100
b(55) = b(55) / (b(41) + b(42) + b(0)) * 100
b(56) = b(56) / (b(41) + b(42) + b(0)) * 100
b(57) = b(54) + b(55) + b(56)
PRINT #1, "NO BUST     ";
PRINT #1, USING "##########.##"; b(54);: PRINT #1, "%  ";
PRINT #1, USING "#########.##"; b(55);: PRINT #1, "%  ";
PRINT #1, USING "#########.##"; b(56);: PRINT #1, "%  ";
PRINT #1, USING "##########.##"; b(57);: PRINT #1, "%  ";
PRINT #1, ""
b(54) = 0
b(55) = b(19)
b(56) = 0
b(57) = b(54) + b(55) + b(56)
b(54) = b(54) / (b(41) + b(42) + b(0)) * 100
b(55) = b(55) / (b(41) + b(42) + b(0)) * 100
b(56) = b(56) / (b(41) + b(42) + b(0)) * 100
b(57) = b(54) + b(55) + b(56)
PRINT #1, "PLAYER BUST ";
PRINT #1, USING "##########.##"; b(54);: PRINT #1, "%  ";
PRINT #1, USING "#########.##"; b(55);: PRINT #1, "%  ";
PRINT #1, USING "#########.##"; b(56);: PRINT #1, "%  ";
PRINT #1, USING "##########.##"; b(57);: PRINT #1, "%  ";
PRINT #1, ""
b(54) = b(20)
b(55) = 0
b(56) = 0
b(57) = b(54) + b(55) + b(56)
b(54) = b(54) / (b(41) + b(42) + b(0)) * 100
b(55) = b(55) / (b(41) + b(42) + b(0)) * 100
b(56) = b(56) / (b(41) + b(42) + b(0)) * 100
b(57) = b(54) + b(55) + b(56)
PRINT #1, "DEALER BUST ";
PRINT #1, USING "##########.##"; b(54);: PRINT #1, "%  ";
PRINT #1, USING "#########.##"; b(55);: PRINT #1, "%  ";
PRINT #1, USING "#########.##"; b(56);: PRINT #1, "%  ";
PRINT #1, USING "##########.##"; b(57);: PRINT #1, "%  ";
PRINT #1, ""
b(54) = b(45)
b(55) = b(46)
b(56) = b(1)
b(57) = b(54) + b(55) + b(56)
b(54) = b(54) / (b(41) + b(42) + b(0)) * 100
b(55) = b(55) / (b(41) + b(42) + b(0)) * 100
b(56) = b(56) / (b(41) + b(42) + b(0)) * 100
b(57) = b(54) + b(55) + b(56)
PRINT #1, "DOUBLE BUST ";
PRINT #1, USING "##########.##"; b(54);: PRINT #1, "%  ";
PRINT #1, USING "#########.##"; b(55);: PRINT #1, "%  ";
PRINT #1, USING "#########.##"; b(56);: PRINT #1, "%  ";
PRINT #1, USING "##########.##"; b(57);: PRINT #1, "%  ";
PRINT #1, ""
b(54) = b(41)
b(55) = b(42)
b(56) = b(0)
b(57) = b(54) + b(55) + b(56)
b(54) = b(54) / (b(41) + b(42) + b(0)) * 100
b(55) = b(55) / (b(41) + b(42) + b(0)) * 100
b(56) = b(56) / (b(41) + b(42) + b(0)) * 100
b(57) = b(54) + b(55) + b(56)
PRINT #1, "TOTAL       ";
PRINT #1, USING "##########.##"; b(54);: PRINT #1, "%  ";
PRINT #1, USING "#########.##"; b(55);: PRINT #1, "%  ";
PRINT #1, USING "#########.##"; b(56);: PRINT #1, "%  ";
PRINT #1, USING "##########.##"; b(57);: PRINT #1, "%  ";
PRINT #1, ""
PRINT #1, ""
PRINT #1, "The following table displays information gathered during";
PRINT #1, " the simulation about"
PRINT #1, "the number of times the dealer paid the player for ";
PRINT #1, "obtaining each of a set of"
PRINT #1, "user-defined predetermined winning card combinations and ";
PRINT #1, "the bonus payoff if any."
PRINT #1, ""
PRINT #1, "Card Combination      Number             Bonus Payoff"
PRINT #1, ""
PRINT #1, ""
PRINT #1, "blackjack      ";
PRINT #1, USING "#############"; mc21po(13, 1);
PRINT #1, "             ";
PRINT #1, USING "##########.#"; blackjack - 1;
PRINT #1, " : 1 Odds "
IF bjsh% = 1 THEN
    PRINT #1, "blackjack      "
    PRINT #1, "after a split  ";
    PRINT #1, USING "#############"; mc21po(12, 1);
    PRINT #1, "             ";
    PRINT #1, USING "##########.#"; blackjack - 1;
    PRINT #1, " : 1 Odds "
END IF
IF mc21po(4, 0) <> 0 THEN
    PRINT #1, "6 Card 27      ";
    PRINT #1, USING "#############"; mc21po(4, 1);: PRINT #1, "             ";
    PRINT #1, USING "##########.#"; mc21po(4, 0);: IF ptm(1) <> 1 THEN PRINT #1, " : 1 Odds " ELSE PRINT #1, " times table minimum"
END IF
IF mc21po(5, 0) <> 0 THEN
    PRINT #1, "7 Card 27      ";
    PRINT #1, USING "#############"; mc21po(5, 1);: PRINT #1, "             ";
    PRINT #1, USING "##########.#"; mc21po(5, 0);: IF ptm(1) <> 1 THEN PRINT #1, " : 1 Odds " ELSE PRINT #1, " times table minimum"
END IF
IF mc21po(6, 0) <> 0 THEN
    PRINT #1, "8 Card 27      ";
    PRINT #1, USING "#############"; mc21po(6, 1);: PRINT #1, "             ";
    PRINT #1, USING "##########.#"; mc21po(6, 0);: IF ptm(1) <> 1 THEN PRINT #1, " : 1 Odds " ELSE PRINT #1, " times table minimum"
END IF
IF mc21po(7, 0) <> 0 THEN
    PRINT #1, "9 Card 27      ";
    PRINT #1, USING "#############"; mc21po(7, 1);: PRINT #1, "             ";
    PRINT #1, USING "##########.#"; mc21po(7, 0);: IF ptm(1) <> 1 THEN PRINT #1, " : 1 Odds " ELSE PRINT #1, " times table minimum"
    PRINT #1, ""
END IF
IF ms678 <> 0 THEN
    PRINT #1, "678 mixed suits";
    PRINT #1, USING "#############"; mc21po(8, 1);: PRINT #1, "             ";
    PRINT #1, USING "##########.#"; ms678;: IF ptm(2) <> 1 THEN PRINT #1, " : 1 Odds " ELSE PRINT #1, " times table minimum"
END IF
IF ss678 <> 0 THEN
    PRINT #1, "678 same suit  ";
    PRINT #1, USING "#############"; mc21po(9, 1);: PRINT #1, "             ";
    PRINT #1, USING "##########.#"; ss678;: IF ptm(2) <> 1 THEN PRINT #1, " : 1 Odds " ELSE PRINT #1, " times table minimum"
END IF
IF sp678 <> 0 THEN
    PRINT #1, "678 spades suit";
    PRINT #1, USING "#############"; mc21po(10, 1);: PRINT #1, "             ";
    PRINT #1, USING "##########.#"; sp678;: IF ptm(2) <> 1 THEN PRINT #1, " : 1 Odds " ELSE PRINT #1, " times table minimum"
    PRINT #1, ""
END IF
IF ms777 <> 0 THEN
    PRINT #1, "777 mixed suits";
    PRINT #1, USING "#############"; mc21po(1, 1);: PRINT #1, "             ";
    PRINT #1, USING "##########.#"; ms777;: IF ptm(3) <> 1 THEN PRINT #1, " : 1 Odds " ELSE PRINT #1, " times table minimum"
END IF
IF ss777 <> 0 THEN
    PRINT #1, "777 same suit  ";
    PRINT #1, USING "#############"; mc21po(2, 1);: PRINT #1, "             ";
    PRINT #1, USING "##########.#"; ss777;: IF ptm(3) <> 1 THEN PRINT #1, " : 1 Odds " ELSE PRINT #1, " times table minimum"
END IF
IF sp777 <> 0 THEN
    PRINT #1, "777 spades suit";
    PRINT #1, USING "#############"; mc21po(3, 1);: PRINT #1, "             ";
    PRINT #1, USING "##########.#"; sp777;: IF ptm(3) <> 1 THEN PRINT #1, " : 1 Odds " ELSE PRINT #1, " times table minimum"
END IF
IF jp777 <> 0 THEN
    PRINT #1, "ss777+dealer 7 ";
    PRINT #1, USING "#############"; mc21po(11, 1);: PRINT #1, "             ";
    PRINT #1, USING "##########.#"; jp777;: IF ptm(3) <> 1 THEN PRINT #1, " : 1 Odds " ELSE PRINT #1, " times table minimum"
    PRINT #1, ""
END IF
PRINT #1, ""
PRINT #1, ""
PRINT #1, ""
PRINT #1, ""
CLOSE #1
END




