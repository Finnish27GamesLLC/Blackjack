'BJ282.BAS is a software application that enables the user to
'program their computer to conduct complete-combinatorial analyses
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


_TITLE "BJ282.BAS"
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
TYPE bas
    dat AS SINGLE
END TYPE
DIM database AS bas: DIM num AS bs: DIM g(32) AS INTEGER: DIM h(32) AS INTEGER: DIM splithands(10) AS DOUBLE
DIM sd AS bsrules: DIM hit(54, 9) AS SINGLE: DIM sar(54, 9) AS INTEGER: DIM dbj(29) AS DOUBLE: DIM x(5) AS DOUBLE
DIM far(54, 9) AS INTEGER: DIM br(54, 9) AS INTEGER: DIM dadd(14) AS INTEGER: DIM splitprob(9) AS DOUBLE
DIM pmult(14) AS INTEGER: DIM pp(14) AS DOUBLE: DIM padd(14) AS INTEGER: DIM splitdbj(10): DIM aq(54, 9, 32)
DIM dmult(14) AS INTEGER: DIM dp(14) AS DOUBLE: DIM duc(10): DIM f(29) AS DOUBLE: DIM card(22): DIM y(5) AS DOUBLE
DIM mc(40) AS DOUBLE: DIM df(12) AS DOUBLE: DIM mv(5) AS INTEGER: DIM puc(54): DIM mc21po(11) AS SINGLE
DIM pc1 AS INTEGER: DIM pc2 AS INTEGER: DIM dc1 AS INTEGER: DIM pdenom(12) AS INTEGER: DIM pshc(7, 4) AS DOUBLE
GOTO 226
777 pmult(2) = pc1 * pc2: padd(2) = pc1 + pc2 + 2: pp(2) = 1: pp(0) = pp(2): padd(0) = padd(2)
IF dc1 = 0 AND push% <> 1 THEN df(10) = mc(9) / mc(10)
IF dc1 = 9 AND ten% <> 1 THEN df(10) = mc(0) / mc(10)
pp(1) = pp(0) * (1 - df(10))
IF pmult(2) = 0 AND padd(2) > h + 1 THEN padd(0) = padd(0) + 10
IF padd(0) > g - 1 AND dd% = 0 THEN
    IF padd(0) = 21 AND bjsh% = 1 THEN f(I) = f(I) + pp(1) * blackjack: dbj(I) = dbj(I) - pp(0) * df(10): GOTO 18
    IF padd(0) > 21 THEN f(I) = f(I) - pp(1): dbj(I) = dbj(I) - pp(0) * df(10): GOTO 18
    IF padd(0) = 21 AND immed% = 1 THEN f(I) = f(I) + pp(1): dbj(I) = dbj(I) - pp(0) * df(10): GOTO 18
    GOSUB 666: GOTO 18
END IF
IF nodsa% = 1 THEN
    IF padd(0) = 21 AND immed% = 1 THEN f(I) = f(I) + pp(1): dbj(I) = dbj(I) - pp(0) * df(10): GOTO 18
    GOSUB 666: GOTO 18
END IF
FOR s = 0 TO 9 STEP 1
    pmult(3) = pmult(2) * s: padd(3) = padd(2) + s + 1: padd(0) = padd(3)
    IF mc(s) < 1 THEN mc(14) = mc(s) ELSE mc(14) = 1
    IF mc(s) = 0 THEN GOTO 19 ELSE pp(3) = pp(2) * mc(s) / mc(10): pp(0) = pp(3): mc(s) = mc(s) - mc(14): mc(10) = mc(10) - mc(14)
    IF dc1 = 0 AND push% <> 1 THEN df(10) = mc(9) / mc(10)
    IF dc1 = 9 AND ten% <> 1 THEN df(10) = mc(0) / mc(10)
    pp(1) = pp(0) * (1 - df(10))
    IF b21po% = 1 THEN
        IF padd(3) = 18 THEN
            IF pc1 = 6 AND pc2 = 6 THEN
                IF dc1 = 6 THEN po777 = mc21po(1) + mc21po(11) ELSE po777 = mc21po(1) + mc21po(2) + mc21po(3)
                IF po777 <> 0 THEN pp(0) = pp(3) * po777: f(I) = f(I) + pp(1): mc(s) = mc(s) + mc(14): mc(10) = mc(10) + mc(14): dbj(I) = dbj(I) - pp(0) * df(10): GOTO 19
            END IF
            IF pc1 = 5 THEN IF pc2 = 6 OR pc2 = 7 THEN po678 = mc21po(8) + mc21po(9) + mc21po(10): IF po678 <> 0 THEN pp(0) = pp(3) * po678: f(I) = f(I) + pp(1): mc(s) = mc(s) + mc(14): mc(10) = mc(10) + mc(14): dbj(I) = dbj(I) - pp(0) * df(10): GOTO 19
            IF pc1 = 6 THEN IF pc2 = 5 OR pc2 = 7 THEN po678 = mc21po(8) + mc21po(9) + mc21po(10): IF po678 <> 0 THEN pp(0) = pp(3) * po678: f(I) = f(I) + pp(1): mc(s) = mc(s) + mc(14): mc(10) = mc(10) + mc(14): dbj(I) = dbj(I) - pp(0) * df(10): GOTO 19
            IF pc1 = 7 THEN IF pc2 = 5 OR pc2 = 6 THEN po678 = mc21po(8) + mc21po(9) + mc21po(10): IF po678 <> 0 THEN pp(0) = pp(3) * po678: f(I) = f(I) + pp(1): mc(s) = mc(s) + mc(14): mc(10) = mc(10) + mc(14): dbj(I) = dbj(I) - pp(0) * df(10): GOTO 19
        END IF
    END IF
    IF pmult(3) = 0 THEN
        IF padd(3) > h + 1 AND padd(3) < 12 THEN padd(0) = padd(0) + 10
        IF padd(3) < h + 2 AND dd% = 1 THEN padd(0) = padd(0) + 10
    END IF
    IF padd(0) > g - 1 OR dd% = 1 THEN
        IF padd(0) > 21 THEN dbj(I) = dbj(I) - pp(0) * df(10): f(I) = f(I) - pp(1)
        IF padd(0) = 21 AND immed% = 1 THEN dbj(I) = dbj(I) - pp(0) * df(10): f(I) = f(I) + pp(1)
        IF padd(0) = 21 AND immed% <> 1 THEN GOSUB 666
        IF padd(0) < 21 THEN GOSUB 666
        mc(s) = mc(s) + mc(14): mc(10) = mc(10) + mc(14): GOTO 19
    END IF
    FOR t = 0 TO 9 STEP 1
        pmult(4) = pmult(3) * t: padd(4) = padd(3) + t + 1: padd(0) = padd(4)
        IF mc(t) < 1 THEN mc(15) = mc(t) ELSE mc(15) = 1
        IF mc(t) = 0 THEN GOTO 20 ELSE pp(4) = pp(3) * mc(t) / mc(10): pp(0) = pp(4): mc(t) = mc(t) - mc(15): mc(10) = mc(10) - mc(15)
        IF dc1 = 0 AND push% <> 1 THEN df(10) = mc(9) / mc(10)
        IF dc1 = 9 AND ten% <> 1 THEN df(10) = mc(0) / mc(10)
        pp(1) = pp(0) * (1 - df(10))
        IF pmult(4) = 0 AND padd(4) > h + 1 AND padd(4) < 12 THEN padd(0) = padd(0) + 10
        IF padd(0) > g - 1 THEN
            IF padd(0) > 21 THEN dbj(I) = dbj(I) - pp(0) * df(10): f(I) = f(I) - pp(1)
            IF padd(0) = 21 AND immed% = 1 THEN dbj(I) = dbj(I) - pp(0) * df(10): f(I) = f(I) + pp(1)
            IF padd(0) = 21 AND immed% <> 1 THEN GOSUB 666
            IF padd(0) < 21 THEN GOSUB 666
            mc(t) = mc(t) + mc(15): mc(10) = mc(10) + mc(15): GOTO 20
        END IF
        FOR u = 0 TO 9 STEP 1
            pmult(5) = pmult(4) * u: padd(5) = padd(4) + u + 1: padd(0) = padd(5)
            IF mc(u) < 1 THEN mc(16) = mc(u) ELSE mc(16) = 1
            IF mc(u) = 0 THEN GOTO 21 ELSE pp(5) = pp(4) * mc(u) / mc(10): pp(0) = pp(5): mc(u) = mc(u) - mc(16): mc(10) = mc(10) - mc(16)
            IF dc1 = 0 AND push% <> 1 THEN df(10) = mc(9) / mc(10)
            IF dc1 = 9 AND ten% <> 1 THEN df(10) = mc(0) / mc(10)
            pp(1) = pp(0) * (1 - df(10))
            IF pmult(5) = 0 AND padd(5) > h + 1 AND padd(5) < 12 THEN padd(0) = padd(0) + 10
            IF padd(5) > g - 1 THEN
                IF b21po% = 1 AND padd(0) = 21 THEN IF mc21po(4) <> 0 THEN dbj(I) = dbj(I) - pp(0) * df(10): pp(1) = pp(1) * mc21po(4): f(I) = f(I) + pp(1): mc(u) = mc(u) + mc(16): mc(10) = mc(10) + mc(16): GOTO 21
                IF padd(0) > 21 THEN dbj(I) = dbj(I) - pp(0) * df(10): f(I) = f(I) - pp(1)
                IF padd(0) = 21 AND immed% = 1 THEN dbj(I) = dbj(I) - pp(0) * df(10): f(I) = f(I) + pp(1)
                IF padd(0) = 21 AND immed% <> 1 THEN GOSUB 666
                IF padd(0) < 21 THEN GOSUB 666
                mc(u) = mc(u) + mc(16): mc(10) = mc(10) + mc(16): GOTO 21
            END IF
            FOR v = 0 TO 9 STEP 1
                pmult(6) = pmult(5) * v: padd(6) = padd(5) + v + 1: padd(0) = padd(6)
                IF mc(v) < 1 THEN mc(17) = mc(v) ELSE mc(17) = 1
                IF mc(v) = 0 THEN GOTO 22 ELSE pp(6) = pp(5) * mc(v) / mc(10): pp(0) = pp(6): mc(v) = mc(v) - mc(17): mc(10) = mc(10) - mc(17)
                IF dc1 = 0 AND push% <> 1 THEN df(10) = mc(9) / mc(10)
                IF dc1 = 9 AND ten% <> 1 THEN df(10) = mc(0) / mc(10)
                pp(1) = pp(0) * (1 - df(10))
                IF pmult(6) = 0 AND padd(6) > h + 1 AND padd(6) < 10 THEN padd(0) = padd(0) + 10
                IF padd(0) > g - 1 THEN
                    IF b21po% = 1 AND padd(0) = 21 THEN IF mc21po(5) <> 0 THEN dbj(I) = dbj(I) - pp(0) * df(10): pp(1) = pp(1) * mc21po(5): f(I) = f(I) + pp(1): mc(v) = mc(v) + mc(17): mc(10) = mc(10) + mc(17): GOTO 22
                    IF schw% = 1 THEN IF padd(6) < 22 THEN dbj(I) = dbj(I) - pp(0) * df(10): f(I) = f(I) + pp(1): mc(v) = mc(v) + mc(17): mc(10) = mc(10) + mc(17): GOTO 22
                    IF schw% = 1 THEN IF padd(6) > 21 THEN dbj(I) = dbj(I) - pp(0) * df(10): f(I) = f(I) - pp(1): mc(v) = mc(v) + mc(17): mc(10) = mc(10) + mc(17): GOTO 22
                    IF padd(0) > 21 THEN dbj(I) = dbj(I) - pp(0) * df(10): f(I) = f(I) - pp(1)
                    IF padd(0) = 21 AND immed% = 1 THEN dbj(I) = dbj(I) - pp(0) * df(10): f(I) = f(I) + pp(1)
                    IF padd(0) = 21 AND immed% <> 1 THEN GOSUB 666
                    IF padd(0) < 21 THEN GOSUB 666
                    mc(v) = mc(v) + mc(17): mc(10) = mc(10) + mc(17): GOTO 22
                END IF
                IF schw% = 1 THEN IF padd(6) < 22 THEN dbj(I) = dbj(I) - pp(0) * df(10): f(I) = f(I) + pp(1): mc(v) = mc(v) + mc(17): mc(10) = mc(10) + mc(17): GOTO 22
                IF schw% = 1 THEN IF padd(6) > 21 THEN dbj(I) = dbj(I) - pp(0) * df(10): f(I) = f(I) - pp(1): mc(v) = mc(v) + mc(17): mc(10) = mc(10) + mc(17): GOTO 22
                FOR w = 0 TO 9 STEP 1
                    pmult(7) = pmult(6) * w: padd(7) = padd(6) + w + 1: padd(0) = padd(7)
                    IF mc(w) < 1 THEN mc(12) = mc(w) ELSE mc(12) = 1
                    IF mc(w) = 0 THEN GOTO 23 ELSE pp(7) = pp(6) * mc(w) / mc(10): pp(0) = pp(7): mc(w) = mc(w) - mc(12): mc(10) = mc(10) - mc(12)
                    IF dc1 = 0 AND push% <> 1 THEN df(10) = mc(9) / mc(10)
                    IF dc1 = 9 AND ten% <> 1 THEN df(10) = mc(0) / mc(10)
                    pp(1) = pp(0) * (1 - df(10))
                    IF pmult(7) = 0 AND padd(7) > h + 1 AND padd(7) < 12 THEN padd(0) = padd(0) + 10
                    IF padd(0) > g - 1 THEN
                        IF b21po% = 1 AND padd(0) = 21 THEN IF mc21po(6) <> 0 THEN dbj(I) = dbj(I) - pp(0) * df(10): pp(1) = pp(1) * mc21po(6): f(I) = f(I) + pp(1): mc(w) = mc(w) + mc(12): mc(10) = mc(10) + mc(12): GOTO 23
                        IF padd(0) > 21 THEN dbj(I) = dbj(I) - pp(0) * df(10): f(I) = f(I) - pp(1)
                        IF padd(0) = 21 AND immed% = 1 THEN dbj(I) = dbj(I) - pp(0) * df(10): f(I) = f(I) + pp(1)
                        IF padd(0) = 21 AND immed% <> 1 THEN GOSUB 666
                        IF padd(0) < 21 THEN GOSUB 666
                        mc(w) = mc(w) + mc(12): mc(10) = mc(10) + mc(12): GOTO 23
                    END IF
                    FOR o = 0 TO 9 STEP 1
                        pmult(8) = pmult(7) * o: padd(8) = padd(7) + o + 1: padd(0) = padd(8)
                        IF mc(o) < 1 THEN mc(18) = mc(o) ELSE mc(18) = 1
                        IF mc(o) = 0 THEN GOTO 24 ELSE pp(8) = pp(7) * mc(o) / mc(10): pp(0) = pp(8): mc(o) = mc(o) - mc(18): mc(10) = mc(10) - mc(18)
                        IF dc1 = 0 AND push% <> 1 THEN df(10) = mc(9) / mc(10)
                        IF dc1 = 9 AND ten% <> 1 THEN df(10) = mc(0) / mc(10)
                        pp(1) = pp(0) * (1 - df(10))
                        IF pmult(8) = 0 AND padd(8) > h + 1 AND padd(8) < 12 THEN padd(0) = padd(0) + 10
                        IF padd(0) > g - 1 THEN
                            IF b21po% = 1 AND padd(0) = 21 THEN IF mc21po(7) <> 0 THEN dbj(I) = dbj(I) - pp(0) * df(10): pp(1) = pp(1) * mc21po(7): f(I) = f(I) + pp(1): mc(o) = mc(o) + mc(18): mc(10) = mc(10) + mc(18): GOTO 24
                            IF padd(0) > 21 THEN dbj(I) = dbj(I) - pp(0) * df(10): f(I) = f(I) - pp(1)
                            IF padd(0) = 21 AND immed% = 1 THEN dbj(I) = dbj(I) - pp(0) * df(10): f(I) = f(I) + pp(1)
                            IF padd(0) = 21 AND immed% <> 1 THEN GOSUB 666
                            IF padd(0) < 21 THEN GOSUB 666
                            mc(o) = mc(o) + mc(18): mc(10) = mc(10) + mc(18): GOTO 24
                        END IF
                        FOR aa = 0 TO 9 STEP 1
                            pmult(9) = pmult(8) * aa: padd(9) = padd(8) + aa + 1: padd(0) = padd(9)
                            IF mc(aa) < 1 THEN mc(22) = mc(aa) ELSE mc(22) = 1
                            IF mc(aa) = 0 THEN GOTO 25 ELSE pp(9) = pp(8) * mc(aa) / mc(10): pp(0) = pp(9): mc(aa) = mc(aa) - mc(22): mc(10) = mc(10) - mc(22)
                            IF dc1 = 0 AND push% <> 1 THEN df(10) = mc(9) / mc(10)
                            IF dc1 = 9 AND ten% <> 1 THEN df(10) = mc(0) / mc(10)
                            pp(1) = pp(0) * (1 - df(10))
                            IF pmult(9) = 0 AND padd(9) > h + 1 AND padd(9) < 12 THEN padd(0) = padd(0) + 10
                            IF padd(0) > g - 1 THEN
                                IF b21po% = 1 AND padd(0) = 21 THEN IF mc21po(7) <> 0 THEN dbj(I) = dbj(I) - pp(0) * df(10): pp(1) = pp(1) * mc21po(7): f(I) = f(I) + pp(1): mc(aa) = mc(aa) + mc(22): mc(10) = mc(10) + mc(22): GOTO 25
                                IF padd(0) > 21 THEN dbj(I) = dbj(I) - pp(0) * df(10): f(I) = f(I) - pp(1)
                                IF padd(0) = 21 AND immed% = 1 THEN dbj(I) = dbj(I) - pp(0) * df(10): f(I) = f(I) + pp(1)
                                IF padd(0) = 21 AND immed% <> 1 THEN GOSUB 666
                                IF padd(0) < 21 THEN GOSUB 666
                                mc(aa) = mc(aa) + mc(22): mc(10) = mc(10) + mc(22): GOTO 25
                            END IF
                            FOR ab = 0 TO 9 STEP 1
                                pmult(10) = pmult(9) * ab: padd(10) = padd(9) + ab + 1: padd(0) = padd(10)
                                IF mc(ab) < 1 THEN mc(23) = mc(ab) ELSE mc(23) = 1
                                IF mc(ab) = 0 THEN GOTO 26 ELSE pp(10) = pp(9) * mc(ab) / mc(10): pp(0) = pp(10): mc(ab) = mc(ab) - mc(23): mc(10) = mc(10) - mc(23)
                                IF dc1 = 0 AND push% <> 1 THEN df(10) = mc(9) / mc(10)
                                IF dc1 = 9 AND ten% <> 1 THEN df(10) = mc(0) / mc(10)
                                pp(1) = pp(0) * (1 - df(10))
                                IF pmult(10) = 0 AND padd(10) > h + 1 AND padd(10) < 12 THEN padd(0) = padd(0) + 10
                                IF padd(0) > g - 1 THEN
                                    IF b21po% = 1 AND padd(0) = 21 THEN IF mc21po(7) <> 0 THEN dbj(I) = dbj(I) - pp(0) * df(10): pp(1) = pp(1) * mc21po(7): f(I) = f(I) + pp(1): mc(ab) = mc(ab) + mc(23): mc(10) = mc(10) + mc(23): GOTO 26
                                    IF padd(0) > 21 THEN dbj(I) = dbj(I) - pp(0) * df(10): f(I) = f(I) - pp(1)
                                    IF padd(0) = 21 AND immed% = 1 THEN dbj(I) = dbj(I) - pp(0) * df(10): f(I) = f(I) + pp(1)
                                    IF padd(0) = 21 AND immed% <> 1 THEN GOSUB 666
                                    IF padd(0) < 21 THEN GOSUB 666
                                    mc(ab) = mc(ab) + mc(23): mc(10) = mc(10) + mc(23): GOTO 26
                                END IF
                                FOR ac = 0 TO 9 STEP 1
                                    pmult(11) = pmult(10) * ac: padd(11) = padd(10) + ac + 1: padd(0) = padd(11)
                                    IF mc(ac) < 1 THEN mc(24) = mc(ac) ELSE mc(24) = 1
                                    IF mc(ac) = 0 THEN GOTO 27 ELSE pp(11) = pp(10) * mc(ac) / mc(10): pp(0) = pp(11): mc(ac) = mc(ac) - mc(24): mc(10) = mc(10) - mc(24)
                                    IF dc1 = 0 AND push% <> 1 THEN df(10) = mc(9) / mc(10)
                                    IF dc1 = 9 AND ten% <> 1 THEN df(10) = mc(0) / mc(10)
                                    pp(1) = pp(0) * (1 - df(10))
                                    IF pmult(11) = 0 AND padd(11) > h + 1 AND padd(11) < 12 THEN padd(0) = padd(0) + 10
                                    IF padd(0) > g - 1 THEN
                                        IF b21po% = 1 AND padd(0) = 21 THEN IF mc21po(7) <> 0 THEN dbj(I) = dbj(I) - pp(0) * df(10): pp(1) = pp(1) * mc21po(7): f(I) = f(I) + pp(1): mc(ac) = mc(ac) + mc(24): mc(10) = mc(10) + mc(24): GOTO 27
                                        IF padd(0) > 21 THEN dbj(I) = dbj(I) - pp(0) * df(10): f(I) = f(I) - pp(1)
                                        IF padd(0) = 21 AND immed% = 1 THEN dbj(I) = dbj(I) - pp(0) * df(10): f(I) = f(I) + pp(1)
                                        IF padd(0) = 21 AND immed% <> 1 THEN GOSUB 666
                                        IF padd(0) < 21 THEN GOSUB 666
                                        mc(ac) = mc(ac) + mc(24): mc(10) = mc(10) + mc(24): GOTO 27
                                    END IF
                                    FOR ad = 0 TO 9 STEP 1
                                        pmult(12) = pmult(11) * ad: padd(12) = padd(11) + ad + 1: padd(0) = padd(12)
                                        IF mc(ad) < 1 THEN mc(28) = mc(ad) ELSE mc(28) = 1
                                        IF mc(ad) = 0 THEN GOTO 15 ELSE pp(12) = pp(11) * mc(ad) / mc(10): pp(0) = pp(12): mc(ad) = mc(ad) - mc(28): mc(10) = mc(10) - mc(28)
                                        IF dc1 = 0 AND push% <> 1 THEN df(10) = mc(9) / mc(10)
                                        IF dc1 = 9 AND ten% <> 1 THEN df(10) = mc(0) / mc(10)
                                        pp(1) = pp(0) * (1 - df(10))
                                        IF padd(0) > g - 1 THEN
                                            IF b21po% = 1 AND padd(0) = 21 THEN IF mc21po(7) <> 0 THEN dbj(I) = dbj(I) - pp(0) * df(10): pp(1) = pp(1) * mc21po(7): f(I) = f(I) + pp(1): mc(ad) = mc(ad) + mc(28): mc(10) = mc(10) + mc(28): GOTO 15
                                            IF padd(0) > 21 THEN dbj(I) = dbj(I) - pp(0) * df(10): f(I) = f(I) - pp(1)
                                            IF padd(0) = 21 AND immed% = 1 THEN dbj(I) = dbj(I) - pp(0) * df(10): f(I) = f(I) + pp(1)
                                            IF padd(0) = 21 AND immed% <> 1 THEN GOSUB 666
                                            IF padd(0) < 21 THEN GOSUB 666
                                            mc(ad) = mc(ad) + mc(28): mc(10) = mc(10) + mc(28): GOTO 15
                                        END IF
                                        FOR ae = 0 TO 9 STEP 1
                                            pmult(13) = pmult(12) * ae: padd(13) = padd(12) + ae + 1: padd(0) = padd(13)
                                            IF mc(ae) < 1 THEN mc(29) = mc(ae) ELSE mc(29) = 1
                                            IF mc(ae) = 0 THEN GOTO 16 ELSE pp(13) = pp(12) * mc(ae) / mc(10): pp(0) = pp(13): mc(ae) = mc(ae) - mc(29): mc(10) = mc(10) - mc(29)
                                            IF dc1 = 0 AND push% <> 1 THEN df(10) = mc(9) / mc(10)
                                            IF dc1 = 9 AND ten% <> 1 THEN df(10) = mc(0) / mc(10)
                                            pp(1) = pp(0) * (1 - df(10))
                                            IF padd(13) > g - 1 THEN
                                                IF b21po% = 1 AND padd(0) = 21 THEN IF mc21po(7) <> 0 THEN dbj(I) = dbj(I) - pp(0) * df(10): pp(1) = pp(1) * mc21po(7): f(I) = f(I) + pp(1): mc(ae) = mc(ae) + mc(29): mc(10) = mc(10) + mc(29): GOTO 16
                                                IF padd(0) > 21 THEN dbj(I) = dbj(I) - pp(0) * df(10): f(I) = f(I) - pp(1)
                                                IF padd(0) = 21 AND immed% = 1 THEN dbj(I) = dbj(I) - pp(0) * df(10): f(I) = f(I) + pp(1)
                                                IF padd(0) = 21 AND immed% <> 1 THEN GOSUB 666
                                                IF padd(0) < 21 THEN GOSUB 666
                                                mc(ae) = mc(ae) + mc(29): mc(10) = mc(10) + mc(29): GOTO 16
                                            END IF
                                            FOR af = 0 TO 9 STEP 1
                                                pmult(14) = pmult(13) * af: padd(14) = padd(13) + af + 1: padd(0) = padd(14)
                                                IF mc(af) < 1 THEN mc(30) = mc(af) ELSE mc(30) = 1
                                                IF mc(af) = 0 THEN GOTO 17 ELSE pp(14) = pp(13) * mc(af) / mc(10): pp(0) = pp(14): mc(af) = mc(af) - mc(30): mc(10) = mc(10) - mc(30)
                                                IF dc1 = 0 AND push% <> 1 THEN df(10) = mc(9) / mc(10)
                                                IF dc1 = 9 AND ten% <> 1 THEN df(10) = mc(0) / mc(10)
                                                pp(1) = pp(0) * (1 - df(10))
                                                IF padd(14) > g - 15 THEN
                                                    IF b21po% = 1 AND padd(0) = 21 THEN IF mc21po(7) <> 0 THEN dbj(I) = dbj(I) - pp(0) * df(10): pp(1) = pp(1) * mc21po(7): f(I) = f(I) + pp(1): mc(af) = mc(af) + mc(30): mc(10) = mc(10) + mc(30): GOTO 17
                                                    IF padd(0) > 21 THEN dbj(I) = dbj(I) - pp(0) * df(10): f(I) = f(I) - pp(1)
                                                    IF padd(0) = 21 AND immed% = 1 THEN dbj(I) = dbj(I) - pp(0) * df(10): f(I) = f(I) + pp(1)
                                                    IF padd(0) = 21 AND immed% <> 1 THEN GOSUB 666
                                                    IF padd(0) < 21 THEN GOSUB 666
                                                    mc(af) = mc(af) + mc(30): mc(10) = mc(10) + mc(30): GOTO 17
                                                END IF
                                                mc(af) = mc(af) + mc(30): mc(10) = mc(10) + mc(30)
                                            17 NEXT af
                                            mc(ae) = mc(ae) + mc(29): mc(10) = mc(10) + mc(29)
                                        16 NEXT ae
                                        mc(ad) = mc(ad) + mc(28): mc(10) = mc(10) + mc(28)
                                    15 NEXT ad
                                    mc(ac) = mc(ac) + mc(24): mc(10) = mc(10) + mc(24)
                                27 NEXT ac
                                mc(ab) = mc(ab) + mc(23): mc(10) = mc(10) + mc(23)
                            26 NEXT ab
                            mc(aa) = mc(aa) + mc(22): mc(10) = mc(10) + mc(22)
                        25 NEXT aa
                        mc(o) = mc(o) + mc(18): mc(10) = mc(10) + mc(18)
                    24 NEXT o
                    mc(w) = mc(w) + mc(12): mc(10) = mc(10) + mc(12)
                23 NEXT w
                mc(v) = mc(v) + mc(17): mc(10) = mc(10) + mc(17)
            22 NEXT v
            mc(u) = mc(u) + mc(16): mc(10) = mc(10) + mc(16)
        21 NEXT u
        mc(t) = mc(t) + mc(15): mc(10) = mc(10) + mc(15)
    20 NEXT t
    mc(s) = mc(s) + mc(14): mc(10) = mc(10) + mc(14)
19 NEXT s
18 RETURN
666 dmult(1) = dc1: dadd(1) = dc1 + 1: dadd(0) = dadd(1): dp(1) = 1
FOR jr = 0 TO 9 STEP 1
    dmult(2) = dmult(1) * jr: dadd(2) = dadd(1) + jr + 1: dadd(0) = dadd(2)
    IF mc(jr) < 1 THEN mc(34) = mc(jr) ELSE mc(34) = 1
    IF mc(jr) = 0 THEN GOTO 28 ELSE dp(2) = mc(jr) / mc(10): mc(jr) = mc(jr) - mc(34): mc(10) = mc(10) - mc(34)
    IF dmult(2) = 0 AND dadd(2) > j + 1 THEN dadd(0) = dadd(0) + 10
    IF dadd(0) > 16 THEN
        mc(jr) = mc(jr) + mc(34): mc(10) = mc(10) + mc(34)
        IF dadd(0) > padd(0) AND dadd(0) < 21 THEN f(I) = f(I) - pp(0) * dp(2)
        IF dadd(0) < padd(0) OR dadd(0) > 21 THEN f(I) = f(I) + pp(0) * dp(2)
        IF dadd(0) = 21 THEN
            dbj(I) = dbj(I) - pp(0) * dp(2)
            IF dc1 = 0 AND push% = 1 THEN
                dbj(I) = 0
                IF dadd(0) > padd(0) THEN f(I) = f(I) - pp(0) * dp(2)
            END IF
            IF dc1 = 9 AND ten% = 1 THEN
                dbj(I) = 0
                IF dadd(0) > padd(0) THEN f(I) = f(I) - pp(0) * dp(2)
            END IF
        END IF
        GOTO 28
    END IF
    FOR js = 0 TO 9 STEP 1
        dmult(3) = dmult(2) * js: dadd(3) = dadd(2) + js + 1: dadd(0) = dadd(3)
        IF mc(js) < 1 THEN mc(35) = mc(js) ELSE mc(35) = 1
        IF mc(js) = 0 THEN GOTO 29 ELSE dp(3) = dp(2) * mc(js) / mc(10): mc(js) = mc(js) - mc(35): mc(10) = mc(10) - mc(35)
        IF dmult(3) = 0 AND dadd(3) > j + 1 AND dadd(3) < 12 THEN dadd(0) = dadd(0) + 10
        IF dadd(0) > 16 THEN
            mc(js) = mc(js) + mc(35): mc(10) = mc(10) + mc(35)
            IF dadd(0) > padd(0) AND dadd(0) < 22 THEN f(I) = f(I) - pp(0) * dp(3)
            IF dadd(0) < padd(0) OR dadd(0) > 21 THEN f(I) = f(I) + pp(0) * dp(3)
            GOTO 29
        END IF
        FOR jt = 0 TO 9 STEP 1
            dmult(4) = dmult(3) * jt: dadd(4) = dadd(3) + jt + 1: dadd(0) = dadd(4)
            IF mc(jt) < 1 THEN mc(36) = mc(jt) ELSE mc(36) = 1
            IF mc(jt) = 0 THEN GOTO 30 ELSE dp(4) = dp(3) * mc(jt) / mc(10): mc(jt) = mc(jt) - mc(36): mc(10) = mc(10) - mc(36)
            IF dmult(4) = 0 AND dadd(4) > j + 1 AND dadd(4) < 12 THEN dadd(0) = dadd(0) + 10
            IF dadd(0) > 16 THEN
                mc(jt) = mc(jt) + mc(36): mc(10) = mc(10) + mc(36)
                IF dadd(0) > padd(0) AND dadd(0) < 22 THEN f(I) = f(I) - pp(0) * dp(4)
                IF dadd(0) < padd(0) OR dadd(0) > 21 THEN f(I) = f(I) + pp(0) * dp(4)
                GOTO 30
            END IF
            FOR ju = 0 TO 9 STEP 1
                dmult(5) = dmult(4) * ju: dadd(5) = dadd(4) + ju + 1: dadd(0) = dadd(5)
                IF mc(ju) < 1 THEN mc(37) = mc(ju) ELSE mc(37) = 1
                IF mc(ju) = 0 THEN GOTO 31 ELSE dp(5) = dp(4) * mc(ju) / mc(10): mc(ju) = mc(ju) - mc(37): mc(10) = mc(10) - mc(37)
                IF dmult(5) = 0 AND dadd(5) > j + 1 AND dadd(5) < 12 THEN dadd(0) = dadd(0) + 10
                IF dadd(0) > 16 THEN
                    mc(ju) = mc(ju) + mc(37): mc(10) = mc(10) + mc(37)
                    IF dadd(0) > padd(0) AND dadd(0) < 22 THEN f(I) = f(I) - pp(0) * dp(5)
                    IF dadd(0) < padd(0) OR dadd(0) > 21 THEN f(I) = f(I) + pp(0) * dp(5)
                    GOTO 31
                END IF
                FOR jv = 0 TO 9 STEP 1
                    dmult(6) = dmult(5) * jv: dadd(6) = dadd(5) + jv + 1: dadd(0) = dadd(6)
                    IF mc(jv) < 1 THEN mc(38) = mc(jv) ELSE mc(38) = 1
                    IF mc(jv) = 0 THEN GOTO 32 ELSE dp(6) = dp(5) * mc(jv) / mc(10): mc(jv) = mc(jv) - mc(38): mc(10) = mc(10) - mc(38)
                    IF dmult(6) = 0 AND dadd(6) > j + 1 AND dadd(6) < 12 THEN dadd(0) = dadd(0) + 10
                    IF dadd(0) > 16 THEN
                        mc(jv) = mc(jv) + mc(38): mc(10) = mc(10) + mc(38)
                        IF dadd(0) > padd(0) AND dadd(0) < 22 THEN f(I) = f(I) - pp(0) * dp(6)
                        IF dadd(0) < padd(0) OR dadd(0) > 21 THEN f(I) = f(I) + pp(0) * dp(6)
                        GOTO 32
                    END IF
                    FOR jw = 0 TO 9 STEP 1
                        dmult(7) = dmult(6) * jw: dadd(7) = dadd(6) + jw + 1: dadd(0) = dadd(7)
                        IF mc(jw) < 1 THEN mc(39) = mc(jw) ELSE mc(39) = 1
                        IF mc(jw) = 0 THEN GOTO 33 ELSE dp(7) = dp(6) * mc(jw) / mc(10): mc(jw) = mc(jw) - mc(39): mc(10) = mc(10) - mc(39)
                        IF dmult(7) = 0 AND dadd(7) > j + 1 AND dadd(7) < 12 THEN dadd(0) = dadd(0) + 10
                        IF dadd(0) > 16 THEN
                            mc(jw) = mc(jw) + mc(39): mc(10) = mc(10) + mc(39)
                            IF dadd(0) > padd(0) AND dadd(0) < 22 THEN f(I) = f(I) - pp(0) * dp(7)
                            IF dadd(0) < padd(0) OR dadd(0) > 21 THEN f(I) = f(I) + pp(0) * dp(7)
                            GOTO 33
                        END IF
                        FOR jo = 0 TO 9 STEP 1
                            dmult(8) = dmult(7) * jo: dadd(8) = dadd(7) + jo + 1: dadd(0) = dadd(8)
                            IF mc(jo) < 1 THEN mc(40) = mc(jo) ELSE mc(40) = 1
                            IF mc(jo) = 0 THEN GOTO 34 ELSE dp(8) = dp(7) * mc(jo) / mc(10): mc(jo) = mc(jo) - mc(40): mc(10) = mc(10) - mc(40)
                            IF dmult(8) = 0 AND dadd(8) > j + 1 AND dadd(8) < 12 THEN dadd(0) = dadd(0) + 10
                            IF dadd(0) > 16 THEN
                                mc(jo) = mc(jo) + mc(40): mc(10) = mc(10) + mc(40)
                                IF dadd(0) > padd(0) AND dadd(0) < 22 THEN f(I) = f(I) - pp(0) * dp(8)
                                IF dadd(0) < padd(0) OR dadd(0) > 21 THEN f(I) = f(I) + pp(0) * dp(8)
                                GOTO 34
                            END IF
                            FOR ja = 0 TO 9 STEP 1
                                dmult(9) = dmult(8) * ja: dadd(9) = dadd(8) + ja + 1: dadd(0) = dadd(9)
                                IF mc(ja) < 1 THEN mc(25) = mc(ja) ELSE mc(25) = 1
                                IF mc(ja) = 0 THEN GOTO 35 ELSE dp(9) = dp(8) * mc(ja) / mc(10): mc(ja) = mc(ja) - mc(25): mc(10) = mc(10) - mc(25)
                                IF dmult(9) = 0 AND dadd(9) > j + 1 AND dadd(9) < 12 THEN dadd(0) = dadd(0) + 10
                                IF dadd(0) > 16 THEN
                                    mc(ja) = mc(ja) + mc(25): mc(10) = mc(10) + mc(25)
                                    IF dadd(0) > padd(0) AND dadd(0) < 22 THEN f(I) = f(I) - pp(0) * dp(9)
                                    IF dadd(0) < padd(0) OR dadd(0) > 21 THEN f(I) = f(I) + pp(0) * dp(9)
                                    GOTO 35
                                END IF
                                FOR jb = 0 TO 9 STEP 1
                                    dmult(10) = dmult(9) * jb: dadd(10) = dadd(9) + jb + 1: dadd(0) = dadd(10)
                                    IF mc(jb) < 1 THEN mc(26) = mc(jb) ELSE mc(26) = 1
                                    IF mc(jb) = 0 THEN GOTO 36 ELSE dp(10) = dp(9) * mc(jb) / mc(10): mc(jb) = mc(jb) - mc(26): mc(10) = mc(10) - mc(26)
                                    IF dmult(10) = 0 AND dadd(10) > j + 1 AND dadd(10) < 12 THEN dadd(0) = dadd(0) + 10
                                    IF dadd(0) > 16 THEN
                                        mc(jb) = mc(jb) + mc(26): mc(10) = mc(10) + mc(26)
                                        IF dadd(0) > padd(0) AND dadd(0) < 22 THEN f(I) = f(I) - pp(0) * dp(10)
                                        IF dadd(0) < padd(0) OR dadd(0) > 21 THEN f(I) = f(I) + pp(0) * dp(10)
                                        GOTO 36
                                    END IF
                                    FOR jc = 0 TO 9 STEP 1
                                        dmult(11) = dmult(10) * jc: dadd(11) = dadd(10) + jc + 1: dadd(0) = dadd(11)
                                        IF mc(jc) < 1 THEN mc(27) = mc(jc) ELSE mc(27) = 1
                                        IF mc(jc) = 0 THEN GOTO 37 ELSE dp(11) = dp(10) * mc(jc) / mc(10): mc(jc) = mc(jc) - mc(27): mc(10) = mc(10) - mc(27)
                                        IF dmult(11) = 0 AND dadd(11) > j + 1 AND dadd(11) < 12 THEN dadd(0) = dadd(0) + 10
                                        IF dadd(0) > 16 THEN
                                            mc(jc) = mc(jc) + mc(27): mc(10) = mc(10) + mc(27)
                                            IF dadd(0) > padd(0) AND dadd(0) < 22 THEN f(I) = f(I) - pp(0) * dp(11)
                                            IF dadd(0) < padd(0) OR dadd(0) > 21 THEN f(I) = f(I) + pp(0) * dp(11)
                                            GOTO 37
                                        END IF
                                        FOR jd = 0 TO 9 STEP 1
                                            dmult(12) = dmult(11) * jd: dadd(12) = dadd(11) + jd + 1: dadd(0) = dadd(12)
                                            IF mc(jd) < 1 THEN mc(31) = mc(jd) ELSE mc(31) = 1
                                            IF mc(jd) = 0 THEN GOTO 38 ELSE dp(12) = dp(11) * mc(jd) / mc(10): mc(jd) = mc(jd) - mc(31): mc(10) = mc(10) - mc(31)
                                            IF dadd(12) > 16 THEN
                                                mc(jd) = mc(jd) + mc(31): mc(10) = mc(10) + mc(31)
                                                IF dadd(0) > padd(0) AND dadd(0) < 22 THEN f(I) = f(I) - pp(0) * dp(12)
                                                IF dadd(0) < padd(0) OR dadd(0) > 21 THEN f(I) = f(I) + pp(0) * dp(12)
                                                GOTO 38
                                            END IF
                                            FOR je = 0 TO 9 STEP 1
                                                dmult(13) = dmult(12) * je: dadd(13) = dadd(9) + je + 1: dadd(0) = dadd(13)
                                                IF mc(je) < 1 THEN mc(32) = mc(je) ELSE mc(32) = 1
                                                IF mc(je) = 0 THEN GOTO 39 ELSE dp(13) = dp(12) * mc(je) / mc(10): mc(je) = mc(je) - mc(32): mc(10) = mc(10) - mc(32)
                                                IF dadd(13) > 16 THEN
                                                    mc(je) = mc(je) + mc(32): mc(10) = mc(10) + mc(32)
                                                    IF dadd(0) > padd(0) AND dadd(0) < 22 THEN f(I) = f(I) - pp(0) * dp(13)
                                                    IF dadd(0) < padd(0) OR dadd(0) > 21 THEN f(I) = f(I) + pp(0) * dp(13)
                                                    GOTO 39
                                                END IF
                                                FOR jf = 0 TO 9 STEP 1
                                                    dmult(14) = dmult(13) * jf: dadd(14) = dadd(13) + jf + 1: dadd(0) = dadd(14)
                                                    IF mc(jf) < 1 THEN mc(33) = mc(jf) ELSE mc(33) = 1
                                                    IF mc(jf) = 0 THEN GOTO 40 ELSE dp(14) = dp(13) * mc(jf) / mc(10): mc(jf) = mc(jf) - mc(33): mc(10) = mc(10) - mc(33)
                                                    IF dadd(14) > 16 THEN
                                                        mc(jf) = mc(jf) + mc(33): mc(10) = mc(10) + mc(33)
                                                        IF dadd(0) > padd(0) AND dadd(0) < 22 THEN f(I) = f(I) - pp(0) * dp(14)
                                                        IF dadd(0) < padd(0) OR dadd(0) > 21 THEN f(I) = f(I) + pp(0) * dp(14)
                                                        GOTO 40
                                                    END IF
                                                    mc(jf) = mc(jf) + mc(33): mc(10) = mc(10) + mc(33)
                                                40 NEXT jf
                                                mc(je) = mc(je) + mc(32): mc(10) = mc(10) + mc(32)
                                            39 NEXT je
                                            mc(jd) = mc(jd) + mc(31): mc(10) = mc(10) + mc(31)
                                        38 NEXT jd
                                        mc(jc) = mc(jc) + mc(27): mc(10) = mc(10) + mc(27)
                                    37 NEXT jc
                                    mc(jb) = mc(jb) + mc(26): mc(10) = mc(10) + mc(26)
                                36 NEXT jb
                                mc(ja) = mc(ja) + mc(25): mc(10) = mc(10) + mc(25)
                            35 NEXT ja
                            mc(jo) = mc(jo) + mc(40): mc(10) = mc(10) + mc(40)
                        34 NEXT jo
                        mc(jw) = mc(jw) + mc(39): mc(10) = mc(10) + mc(39)
                    33 NEXT jw
                    mc(jv) = mc(jv) + mc(38): mc(10) = mc(10) + mc(38)
                32 NEXT jv
                mc(ju) = mc(ju) + mc(37): mc(10) = mc(10) + mc(37)
            31 NEXT ju
            mc(jt) = mc(jt) + mc(36): mc(10) = mc(10) + mc(36)
        30 NEXT jt
        mc(js) = mc(js) + mc(35): mc(10) = mc(10) + mc(35)
    29 NEXT js
    mc(jr) = mc(jr) + mc(34): mc(10) = mc(10) + mc(34)
28 NEXT jr
RETURN
555 dmult(1) = dc1: dadd(1) = dc1 + 1: dadd(0) = dadd(1): dp(1) = 1: ERASE df: account# = 0
FOR jr = 0 TO 9 STEP 1
    dmult(2) = dmult(1) * jr: dadd(2) = dadd(1) + jr + 1: dadd(0) = dadd(2)
    IF mc(jr) < 1 THEN mc(34) = mc(jr) ELSE mc(34) = 1
    IF mc(jr) = 0 THEN GOTO 1028 ELSE dp(2) = mc(jr) / mc(10): mc(jr) = mc(jr) - mc(34): mc(10) = mc(10) - mc(34)
    IF dmult(2) = 0 AND dadd(2) > j + 1 THEN
        IF dadd(0) <> 11 THEN dadd(0) = dadd(0) + 10
        IF dadd(0) = 11 THEN df(10) = df(10) + dp(2): mc(jr) = mc(jr) + mc(34): mc(10) = mc(10) + mc(34): GOTO 1028
    END IF
    IF dadd(0) > 16 THEN mv(1) = dadd(0) - 17: df(mv(1)) = df(mv(1)) + dp(2): mc(jr) = mc(jr) + mc(34): mc(10) = mc(10) + mc(34): GOTO 1028
    FOR js = 0 TO 9 STEP 1
        dmult(3) = dmult(2) * js: dadd(3) = dadd(2) + js + 1: dadd(0) = dadd(3)
        IF mc(js) < 1 THEN mc(35) = mc(js) ELSE mc(35) = 1
        IF mc(js) = 0 THEN GOTO 1029 ELSE dp(3) = dp(2) * mc(js) / mc(10): mc(js) = mc(js) - mc(35): mc(10) = mc(10) - mc(35)
        IF dmult(3) = 0 AND dadd(3) > j + 1 AND dadd(3) < 12 THEN dadd(0) = dadd(0) + 10
        IF dadd(0) > 16 THEN mv(1) = dadd(0) - 17: df(mv(1)) = df(mv(1)) + dp(3): mc(js) = mc(js) + mc(35): mc(10) = mc(10) + mc(35): GOTO 1029
        FOR jt = 0 TO 9 STEP 1
            dmult(4) = dmult(3) * jt: dadd(4) = dadd(3) + jt + 1: dadd(0) = dadd(4)
            IF mc(jt) < 1 THEN mc(36) = mc(jt) ELSE mc(36) = 1
            IF mc(jt) = 0 THEN GOTO 1030 ELSE dp(4) = dp(3) * mc(jt) / mc(10): mc(jt) = mc(jt) - mc(36): mc(10) = mc(10) - mc(36)
            IF dmult(4) = 0 AND dadd(4) > j + 1 AND dadd(4) < 12 THEN dadd(0) = dadd(0) + 10
            IF dadd(0) > 16 THEN mv(1) = dadd(0) - 17: df(mv(1)) = df(mv(1)) + dp(4): mc(jt) = mc(jt) + mc(36): mc(10) = mc(10) + mc(36): GOTO 1030
            FOR ju = 0 TO 9 STEP 1
                dmult(5) = dmult(4) * ju: dadd(5) = dadd(4) + ju + 1: dadd(0) = dadd(5)
                IF mc(ju) < 1 THEN mc(37) = mc(ju) ELSE mc(37) = 1
                IF mc(ju) = 0 THEN GOTO 1031 ELSE dp(5) = dp(4) * mc(ju) / mc(10): mc(ju) = mc(ju) - mc(37): mc(10) = mc(10) - mc(37)
                IF dmult(5) = 0 AND dadd(5) > j + 1 AND dadd(5) < 12 THEN dadd(0) = dadd(0) + 10
                IF dadd(0) > 16 THEN mv(1) = dadd(0) - 17: df(mv(1)) = df(mv(1)) + dp(5): mc(ju) = mc(ju) + mc(37): mc(10) = mc(10) + mc(37): GOTO 1031
                FOR jv = 0 TO 9 STEP 1
                    dmult(6) = dmult(5) * jv: dadd(6) = dadd(5) + jv + 1: dadd(0) = dadd(6)
                    IF mc(jv) < 1 THEN mc(38) = mc(jv) ELSE mc(38) = 1
                    IF mc(jv) = 0 THEN GOTO 1032 ELSE dp(6) = dp(5) * mc(jv) / mc(10): mc(jv) = mc(jv) - mc(38): mc(10) = mc(10) - mc(38)
                    IF dmult(6) = 0 AND dadd(6) > j + 1 AND dadd(6) < 12 THEN dadd(0) = dadd(0) + 10
                    IF dadd(0) > 16 THEN mv(1) = dadd(0) - 17: df(mv(1)) = df(mv(1)) + dp(6): mc(jv) = mc(jv) + mc(38): mc(10) = mc(10) + mc(38): GOTO 1032
                    FOR jw = 0 TO 9 STEP 1
                        dmult(7) = dmult(6) * jw: dadd(7) = dadd(6) + jw + 1: dadd(0) = dadd(7)
                        IF mc(jw) < 1 THEN mc(39) = mc(jw) ELSE mc(39) = 1
                        IF mc(jw) = 0 THEN GOTO 1033 ELSE dp(7) = dp(6) * mc(jw) / mc(10): mc(jw) = mc(jw) - mc(39): mc(10) = mc(10) - mc(39)
                        IF dmult(7) = 0 AND dadd(7) > j + 1 AND dadd(7) < 12 THEN dadd(0) = dadd(0) + 10
                        IF dadd(0) > 16 THEN mv(1) = dadd(0) - 17: df(mv(1)) = df(mv(1)) + dp(7): mc(jw) = mc(jw) + mc(39): mc(10) = mc(10) + mc(39): GOTO 1033
                        FOR jo = 0 TO 9 STEP 1
                            dmult(8) = dmult(7) * jo: dadd(8) = dadd(7) + jo + 1: dadd(0) = dadd(8)
                            IF mc(jo) < 1 THEN mc(40) = mc(jo) ELSE mc(40) = 1
                            IF mc(jo) = 0 THEN GOTO 1034 ELSE dp(8) = dp(7) * mc(jo) / mc(10): mc(jo) = mc(jo) - mc(40): mc(10) = mc(10) - mc(40)
                            IF dmult(8) = 0 AND dadd(8) > j + 1 AND dadd(8) < 12 THEN dadd(0) = dadd(0) + 10
                            IF dadd(0) > 16 THEN mv(1) = dadd(0) - 17: df(mv(1)) = df(mv(1)) + dp(8): mc(jo) = mc(jo) + mc(40): mc(10) = mc(10) + mc(40): GOTO 1034
                            FOR ja = 0 TO 9 STEP 1
                                dmult(9) = dmult(8) * ja: dadd(9) = dadd(8) + ja + 1: dadd(0) = dadd(9)
                                IF mc(ja) < 1 THEN mc(25) = mc(ja) ELSE mc(25) = 1
                                IF mc(ja) = 0 THEN GOTO 1035 ELSE dp(9) = dp(8) * mc(ja) / mc(10): mc(ja) = mc(ja) - mc(25): mc(10) = mc(10) - mc(25)
                                IF dmult(9) = 0 AND dadd(9) > j + 1 AND dadd(9) < 12 THEN dadd(0) = dadd(0) + 10
                                IF dadd(0) > 16 THEN mv(1) = dadd(0) - 17: df(mv(1)) = df(mv(1)) + dp(9): mc(ja) = mc(ja) + mc(25): mc(10) = mc(10) + mc(25): GOTO 1035
                                FOR jb = 0 TO 9 STEP 1
                                    dmult(10) = dmult(9) * jb: dadd(10) = dadd(9) + jb + 1: dadd(0) = dadd(10)
                                    IF mc(jb) < 1 THEN mc(26) = mc(jb) ELSE mc(26) = 1
                                    IF mc(jb) = 0 THEN GOTO 1036 ELSE dp(10) = dp(9) * mc(jb) / mc(10): mc(jb) = mc(jb) - mc(26): mc(10) = mc(10) - mc(26)
                                    IF dmult(10) = 0 AND dadd(10) > j + 1 AND dadd(10) < 12 THEN dadd(0) = dadd(0) + 10
                                    IF dadd(0) > 16 THEN mv(1) = dadd(0) - 17: df(mv(1)) = df(mv(1)) + dp(10): mc(jb) = mc(jb) + mc(26): mc(10) = mc(10) + mc(26): GOTO 1036
                                    FOR jc = 0 TO 9 STEP 1
                                        dmult(11) = dmult(10) * jc: dadd(11) = dadd(10) + jc + 1: dadd(0) = dadd(11)
                                        IF mc(jc) < 1 THEN mc(27) = mc(jc) ELSE mc(27) = 1
                                        IF mc(jc) = 0 THEN GOTO 1037 ELSE dp(11) = dp(10) * mc(jc) / mc(10): mc(jc) = mc(jc) - mc(27): mc(10) = mc(10) - mc(27)
                                        IF dmult(11) = 0 AND dadd(11) > j + 1 AND dadd(11) < 12 THEN dadd(0) = dadd(0) + 10
                                        IF dadd(0) > 16 THEN mv(1) = dadd(0) - 17: df(mv(1)) = df(mv(1)) + dp(11): mc(jc) = mc(jc) + mc(27): mc(10) = mc(10) + mc(27): GOTO 1037
                                        FOR jd = 0 TO 9 STEP 1
                                            dmult(12) = dmult(11) * jd: dadd(12) = dadd(11) + jd + 1: dadd(0) = dadd(12)
                                            IF mc(jd) < 1 THEN mc(31) = mc(jd) ELSE mc(31) = 1
                                            IF mc(jd) = 0 THEN GOTO 1038 ELSE dp(12) = dp(11) * mc(jd) / mc(10): mc(jd) = mc(jd) - mc(31): mc(10) = mc(10) - mc(31)
                                            IF dadd(12) > 16 THEN mv(1) = dadd(0) - 17: df(mv(1)) = df(mv(1)) + dp(12): mc(jd) = mc(jd) + mc(31): mc(10) = mc(10) + mc(31): GOTO 1038
                                            FOR je = 0 TO 9 STEP 1
                                                dmult(13) = dmult(12) * je: dadd(13) = dadd(9) + je + 1: dadd(0) = dadd(13)
                                                IF mc(je) < 1 THEN mc(32) = mc(je) ELSE mc(32) = 1
                                                IF mc(je) = 0 THEN GOTO 1039 ELSE dp(13) = dp(12) * mc(je) / mc(10): mc(je) = mc(je) - mc(32): mc(10) = mc(10) - mc(32)
                                                IF dadd(13) > 16 THEN mv(1) = dadd(0) - 17: df(mv(1)) = df(mv(1)) + dp(13): mc(je) = mc(je) + mc(32): mc(10) = mc(10) + mc(32): GOTO 1039
                                                FOR jf = 0 TO 9 STEP 1
                                                    dmult(14) = dmult(13) * jf: dadd(14) = dadd(13) + jf + 1: dadd(0) = dadd(14)
                                                    IF mc(jf) < 1 THEN mc(33) = mc(jf) ELSE mc(33) = 1
                                                    IF mc(jf) = 0 THEN GOTO 1040 ELSE dp(14) = dp(13) * mc(jf) / mc(10): mc(jf) = mc(jf) - mc(33): mc(10) = mc(10) - mc(33)
                                                    IF dadd(14) > 16 THEN mv(1) = dadd(0) - 17: df(mv(1)) = df(mv(1)) + dp(14): mc(jf) = mc(jf) + mc(33): mc(10) = mc(10) + mc(33): GOTO 1040
                                                    mc(jf) = mc(jf) + mc(33): mc(10) = mc(10) + mc(33)
                                                1040 NEXT jf
                                                mc(je) = mc(je) + mc(32): mc(10) = mc(10) + mc(32)
                                            1039 NEXT je
                                            mc(jd) = mc(jd) + mc(31): mc(10) = mc(10) + mc(31)
                                        1038 NEXT jd
                                        mc(jc) = mc(jc) + mc(27): mc(10) = mc(10) + mc(27)
                                    1037 NEXT jc
                                    mc(jb) = mc(jb) + mc(26): mc(10) = mc(10) + mc(26)
                                1036 NEXT jb
                                mc(ja) = mc(ja) + mc(25): mc(10) = mc(10) + mc(25)
                            1035 NEXT ja
                            mc(jo) = mc(jo) + mc(40): mc(10) = mc(10) + mc(40)
                        1034 NEXT jo
                        mc(jw) = mc(jw) + mc(39): mc(10) = mc(10) + mc(39)
                    1033 NEXT jw
                    mc(jv) = mc(jv) + mc(38): mc(10) = mc(10) + mc(38)
                1032 NEXT jv
                mc(ju) = mc(ju) + mc(37): mc(10) = mc(10) + mc(37)
            1031 NEXT ju
            mc(jt) = mc(jt) + mc(36): mc(10) = mc(10) + mc(36)
        1030 NEXT jt
        mc(js) = mc(js) + mc(35): mc(10) = mc(10) + mc(35)
    1029 NEXT js
    mc(jr) = mc(jr) + mc(34): mc(10) = mc(10) + mc(34)
1028 NEXT jr
FOR y = 0 TO 10
df(11) = df(11) + df(y): NEXT y
FOR y = 1 TO 5
df(12) = df(12) + df(4 + y): NEXT y
RETURN
888 IF pc1 <= pc2 THEN n = pc1 / 2 * (19 - pc1) + pc2 ELSE n = pc2 / 2 * (19 - pc2) + pc1
x = 0
IF split% <> 1 THEN hit(n, dc1) = -3 ELSE splithands(pc2) = -3
FOR v = 12 TO 19
    FOR w = 5 TO 8
        IF w + 12 < v THEN w = v - 12
        x = x + 1
        IF v >= hl% AND v <= hu% AND w + 12 >= sl% AND w + 12 <= su% THEN
            IF split% <> 1 THEN
                q# = f(x) + dbj(x)
                aq(n, dc1, x) = q#
                IF q# > hit(n, dc1) THEN
                    IF v > pc1 + pc2 + 1 THEN hit(n, dc1) = q#: br(n, dc1) = x
                    IF v = pc1 + pc2 + 1 AND v = 19 THEN hit(n, dc1) = q#: br(n, dc1) = x
                END IF
            END IF
            IF split% = 1 THEN
                q# = f(x)
                IF n = 9 AND bjsh% = 1 THEN q# = q# * blackjack
                IF hole% = 1 THEN q# = q# + dbj(x)
                IF pc1 > pc2 THEN
                    IF hole% <> 1 AND q# + dbj(x) > splithands(pc2) + splitdbj(pc2) THEN
                        IF v > pc1 + pc2 + 1 THEN splithands(pc2) = q#: splitdbj(pc2) = dbj(x): far(n, dc1) = x
                        IF v = pc1 + pc2 + 1 AND v = 19 THEN splithands(pc2) = q#: splitdbj(pc2) = dbj(x): far(n, dc1) = x
                    END IF
                    IF hole% = 1 AND q# > splithands(pc2) THEN
                        IF v > pc1 + pc2 + 1 THEN splithands(pc2) = q#: splitdbj(pc2) = dbj(x): far(n, dc1) = x
                        IF v = pc1 + pc2 + 1 AND v = 19 THEN splithands(pc2) = q#: splitdbj(pc2) = dbj(x): far(n, dc1) = x
                    END IF
                END IF
                IF pc2 = pc1 THEN
                    IF hole% <> 1 AND q# + dbj(x) > splithands(pc2) + splitdbj(pc2) THEN
                        IF v > pc1 + pc2 + 1 THEN splithands(pc2) = q#: splitdbj(pc2) = dbj(x): far(n, dc1) = x: sar(n, dc1) = x
                        IF v = pc1 + pc2 + 1 AND v = 19 THEN splithands(pc2) = q#: splitdbj(pc2) = dbj(x): far(n, dc1) = x: sar(n, dc1) = x
                    END IF
                    IF hole% = 1 AND q# > splithands(pc2) THEN
                        IF v > pc1 + pc2 + 1 THEN splithands(pc2) = q#: splitdbj(pc2) = dbj(x): far(n, dc1) = x: sar(n, dc1) = x
                        IF v = pc1 + pc2 + 1 AND v = 19 THEN splithands(pc2) = q#: splitdbj(pc2) = dbj(x): far(n, dc1) = x: sar(n, dc1) = x
                    END IF
                END IF
                IF pc1 < pc2 THEN
                    IF hole% <> 1 AND q# + dbj(x) > splithands(pc2) + splitdbj(pc2) THEN
                        IF v > pc1 + pc2 + 1 THEN splithands(pc2) = q#: splitdbj(pc2) = dbj(x): sar(n, dc1) = x
                        IF v = pc1 + pc2 + 1 AND v = 19 THEN splithands(pc2) = q#: splitdbj(pc2) = dbj(x): sar(n, dc1) = x
                    END IF
                    IF hole% = 1 AND q# > splithands(pc2) THEN
                        IF v > pc1 + pc2 + 1 THEN splithands(pc2) = q#: splitdbj(pc2) = dbj(x): sar(n, dc1) = x
                        IF v = pc1 + pc2 + 1 AND v = 19 THEN splithands(pc2) = q#: splitdbj(pc2) = dbj(x): sar(n, dc1) = x
                    END IF
                END IF
            END IF
            IF pc1 * pc2 = 0 THEN
                IF pc1 + pc2 > 5 AND w < 6 THEN
                    IF x < 25 THEN
                        x = x + 1: w = w + 1
                        IF split% <> 1 THEN
                            aq(n, dc1, x) = q#
                        END IF
                    END IF
                END IF
                IF pc1 + pc2 > 6 AND w < 7 THEN
                    IF x < 28 THEN
                        x = x + 1: w = w + 1
                        IF split% <> 1 THEN
                            aq(n, dc1, x) = q#
                        END IF
                    END IF
                END IF
                IF pc1 + pc2 > 7 AND w < 8 THEN
                    x = x + 1: w = w + 1
                    IF split% <> 1 THEN
                        aq(n, dc1, x) = q#
                    END IF
                END IF
            END IF
            IF pc1 * pc2 <> 0 THEN
                IF pc1 + pc2 > 4 AND w < 6 THEN
                    IF x < 25 THEN
                        x = x + 1: w = w + 1
                        IF split% <> 1 THEN
                            aq(n, dc1, x) = q#
                        END IF
                    END IF
                END IF
                IF pc1 + pc2 > 5 AND w < 7 THEN
                    IF x < 28 THEN
                        x = x + 1: w = w + 1
                        IF split% <> 1 THEN
                            aq(n, dc1, x) = q#
                        END IF
                    END IF
                END IF
                IF pc1 + pc2 > 6 AND w < 8 THEN
                    x = x + 1: w = w + 1
                    IF split% <> 1 THEN
                        aq(n, dc1, x) = q#
                    END IF
                END IF
            END IF
        END IF
NEXT w: NEXT v: account# = 0: q# = 0
IF split% <> 1 THEN standr% = br(n, dc1): stand = hit(n, dc1)
RETURN
222 ERASE mc
FOR x = 0 TO 12
    IF x > 8 THEN mc(9) = mc(9) + card(x) * deck% ELSE mc(x) = card(x) * deck%
NEXT x
mc(10) = card(13) * deck%
IF split% = 1 THEN IF mc(pc1) < 1 THEN mc(19) = mc(pc1) ELSE mc(19) = 1
IF split% = 1 THEN IF mc(pc1) = 0 THEN GOTO 228 ELSE mc(pc1) = mc(pc1) - mc(19): mc(10) = mc(10) - mc(19)
IF mc(pc1) < 1 THEN mc(19) = mc(pc1) ELSE mc(19) = 1
IF mc(pc1) = 0 THEN GOTO 228 ELSE mc(pc1) = mc(pc1) - mc(19): mc(10) = mc(10) - mc(19)
IF mc(pc2) < 1 THEN mc(19) = mc(pc2) ELSE mc(19) = 1
IF mc(pc2) = 0 THEN GOTO 228 ELSE mc(pc2) = mc(pc2) - mc(19): mc(10) = mc(10) - mc(19)
IF mc(dc1) < 1 THEN mc(19) = mc(dc1) ELSE mc(19) = 1
IF mc(dc1) = 0 THEN GOTO 228 ELSE mc(dc1) = mc(dc1) - mc(19): mc(10) = mc(10) - mc(19)
mc0 = mc(0): mc9 = mc(9): mc10 = mc(10)
I = 0: dd% = 1: g = 19: h = 8: qdbj# = 0: q# = 0: ERASE f: ERASE dbj
GOSUB 777: q# = q# + f(0)
228 dd% = 0
IF dc1 = 0 AND push% = 1 THEN dbj(0) = 0
IF dc1 = 9 AND ten% = 1 THEN dbj(0) = 0
IF split% <> 1 THEN
    IF hole% <> 1 THEN q# = 2 * q# + dbj(0)
    IF hole% = 1 THEN q# = 2 * (q# + dbj(0))
    aq(n, dc1, 32) = q#
    IF q# > hit(n, dc1) THEN hit(n, dc1) = q#: br(n, dc1) = 32
    ddown = q#
END IF
IF split% = 1 THEN
    IF hole% <> 1 THEN q# = 2 * q#
    IF hole% = 1 THEN q# = 2 * (q# + dbj(0))
    IF pc1 > pc2 THEN
        IF hole% <> 1 AND q# + dbj(0) > splithands(pc2) + splitdbj(pc2) THEN
            splithands(pc2) = q#
            splitdbj(pc2) = dbj(0)
            far(n, dc1) = 32
        END IF
        IF hole% = 1 AND q# > splithands(pc2) THEN
            splithands(pc2) = q#
            splitdbj(pc2) = dbj(0)
            far(n, dc1) = 32
        END IF
    END IF
    IF pc2 = pc1 THEN
        IF hole% <> 1 AND q# + dbj(0) > splithands(pc2) + splitdbj(pc2) THEN
            splithands(pc2) = q#
            splitdbj(pc2) = dbj(0)
            far(n, dc1) = 32: sar(n, dc1) = 32
        END IF
        IF hole% = 1 AND q# > splithands(pc2) THEN
            splithands(pc2) = q#
            splitdbj(pc2) = dbj(0)
            far(n, dc1) = 32: sar(n, dc1) = 32
        END IF
    END IF
    IF pc2 > pc1 THEN
        IF hole% <> 1 AND q# + dbj(0) > splithands(pc2) + splitdbj(pc2) THEN
            splithands(pc2) = q#
            splitdbj(pc2) = dbj(0)
            sar(n, dc1) = 32
        END IF
        IF hole% = 1 AND q# > splithands(pc2) THEN
            splithands(pc2) = q#
            splitdbj(pc2) = dbj(0)
            sar(n, dc1) = 32
        END IF
    END IF
END IF
RETURN
234 IF (split% <> 1 AND ls% = 1) THEN
    q# = -.5
    IF dc1 = 0 AND early% <> 1 THEN q# = q# * (1 - mc9 / mc10) - mc9 / mc10
    IF dc1 = 9 AND early% <> 1 THEN q# = q# * (1 - mc0 / mc10) - mc0 / mc10
    aq(n, dc1, 31) = q#
    IF q# > hit(n, dc1) THEN hit(n, dc1) = q#: br(n, dc1) = 31
    srndr = q#
END IF
RETURN
404 REM Player is allowed to split to 2 post-split hands.
FOR x = 0 TO 0
    x(x) = mc(pc1) - x
NEXT x
FOR y = 0 TO 2
    y(y) = mc(10) - y
NEXT y
pshc(1, 0) = 1 - x(0) / y(0)
pshc(1, 3) = pshc(1, 0) * 1
pshc(1, 4) = pshc(1, 0) * 0
pshc(2, 0) = x(0) / y(0)  
pshc(2, 3) = pshc(2, 0) * 1
pshc(2, 4) = pshc(2, 0) * 1
FOR z = 1 TO 2
    pshc(0, 3) = pshc(0, 3) + pshc(z, 3)
    pshc(0, 4) = pshc(0, 4) + pshc(z, 4)
NEXT z
RETURN
505 REM Player is allowed to split to 3 post-split hands. First post-split-initial hand is not a pair.
FOR x = 0 TO 2
    x(x) = mc(pc1) - x
NEXT x
FOR y = 0 TO 2
    y(y) = mc(10) - y
NEXT y
pshc(1, 0) = 1 - x(0) / y(0)
pshc(1, 3) = pshc(1, 0) * 1
pshc(1, 4) = pshc(1, 0) * 0
pshc(2, 0) = x(0) / y(0)  * (1 - x(1) / y(1)) * (1 - x(1) / y(2))
pshc(2, 3) = pshc(2, 0) * 3
pshc(2, 4) = pshc(2, 0) * 1
pshc(3, 0) = 2 * x(0) / y(0)  * (1 - x(1) / y(1))  *  x(1) / y(2)
pshc(3, 3) = pshc(3, 0) * 3
pshc(3, 4) = pshc(3, 0) * 2
pshc(4, 0) = x(0) / y(0)  * x(1) / y(1)  *  x(1) / y(2) 
pshc(4, 3) = pshc(4, 0) * 3
pshc(4, 4) = pshc(4, 0) * 3
FOR z = 1 TO 4
    pshc(0, 3) = pshc(0, 3) + pshc(z, 3)
    pshc(0, 4) = pshc(0, 4) + pshc(z, 4)
NEXT z
RETURN
808 REM Player is allowed to split to 3 post-split hands. First post-split-initial hand is a pair.
FOR x = 0 TO 2
    x(x) = mc(pc1) - x
NEXT x
FOR y = 0 TO 2
    y(y) = mc(10) - y
NEXT y
pshc(1, 0) = (1 - x(0) / y(0)) * (1 - x(0) / y(1)) * (1 - x(0) / y(2)) 
pshc(1, 3) = pshc(1, 0) * 3
pshc(1, 4) = pshc(1, 0) * 0
pshc(2, 0) = 3 * (1 - x(0) / y(0)) * (1 - x(0) / y(1)) * x(0) / y(2)
pshc(2, 3) = pshc(2, 0) * 3
pshc(2, 4) = pshc(2, 0) * 1
pshc(3, 0) = 3 * (1 - x(0) / y(0)) * x(0) / y(1) *  x(1) / y(2) 
pshc(3, 3) = pshc(3, 0) * 3
pshc(3, 4) = pshc(3, 0) * 2
pshc(4, 0) = x(0) / y(0) * x(1) / y(1) * x(2) / y(2)
pshc(4, 3) = pshc(4, 0) * 3
pshc(4, 4) = pshc(4, 0) * 3
FOR z = 1 TO 4
    pshc(0, 3) = pshc(0, 3) + pshc(z, 3)
    pshc(0, 4) = pshc(0, 4) + pshc(z, 4)
NEXT z
RETURN
1414 REM Player is allowed to split to 4 post-split hands. First post-split-initial hand is not a pair.
FOR x = 0 TO 4
    x(x) = mc(pc1) - x
NEXT x
FOR y = 0 TO 4
    y(y) = mc(10) - y
NEXT y
pshc(1, 0) = 1 - x(0) / y(0)
pshc(1, 3) = pshc(1, 0) * 1
pshc(1, 4) = pshc(1, 0) * 0
pshc(2, 0) = x(0) / y(0)  * (1 - x(1) / y(1)) * (1 - x(1) / y(2))
pshc(2, 3) = pshc(2, 0) * 3
pshc(2, 4) = pshc(2, 0) * 1
pshc(3, 0) = 2 * x(0) / y(0)  * x(1) / y(1)  * (1 - x(2) / y(2)) * (1 - x(2) / y(3))  * (1 - x(2) / y(4)) 
pshc(3, 3) = pshc(3, 0) * 5
pshc(3, 4) = pshc(3, 0) * 2
pshc(4, 0) = 5 * x(0) / y(0) *  x(1) / y(1) * x(2) / y(2) * (1 - x(3) / y(3)) * (1 - x(3) / y(4))
pshc(4, 3) = pshc(4, 0) * 5
pshc(4, 4) = pshc(4, 0) * 3
pshc(5, 0) = 4 * x(0) / y(0) * x(1) / y(1) * x(2) / y(2) * x(3) / y(3) * (1 - x(4) / y(4))
pshc(5, 3) = pshc(5, 0) * 5
pshc(5, 4) = pshc(5, 0) * 4
pshc(6, 0) = x(0) / y(0) * x(1) / y(1)  *  x(2) / y(2) * x(3) / y(3) * x(4) / y(4) 
pshc(6, 3) = pshc(6, 0) * 5
pshc(6, 4) = pshc(6, 0) * 5
FOR z = 1 TO 6
    pshc(0, 3) = pshc(0, 3) + pshc(z, 3)
    pshc(0, 4) = pshc(0, 4) + pshc(z, 4)
NEXT z
REM PRINT pshc(0, 3); pshc(0, 4): SLEEP 2
RETURN
2929 REM Player is allowed to split to 4 post-split hands. First post-split-initial hand is a pair.
FOR x = 0 TO 4
    x(x) = mc(pc1) - x
NEXT x
FOR y = 0 TO 4
    y(y) = mc(10) - y
NEXT y
pshc(1, 0) = (1 - x(0) / y(0)) * (1 - x(0) / y(1)) * (1 - x(0) / y(2)) 
pshc(1, 3) = pshc(1, 0) * 3
pshc(1, 4) = pshc(1, 0) * 0
pshc(2, 0) = 3 * x(0) / y(0) * (1 - x(1) / y(1)) * (1 - x(1)  / y(2)) * (1 - x(1) / y(3))  * (1 - x(1) / y(4))
pshc(2, 3) = pshc(2, 0) * 5
pshc(2, 4) = pshc(2, 0) * 1
pshc(3, 0) = 9 * x(0) / y(0)  * x(1) / y(1)  * (1 - x(2) / y(2)) * (1 - x(2) / y(3))  * (1 - x(2) / y(4)) 
pshc(3, 3) = pshc(3, 0) * 5
pshc(3, 4) = pshc(3, 0) * 2
pshc(4, 0) = 10 * x(0) / y(0) *  x(1) / y(1) * x(2) / y(2) * (1 - x(3) / y(3)) * (1 - x(3) / y(4))
pshc(4, 3) = pshc(4, 0) * 5
pshc(4, 4) = pshc(4, 0) * 3
pshc(5, 0) = 5 * x(0) / y(0) * x(1) / y(1) * x(2) / y(2) * x(3) / y(3) * (1 - x(4) / y(4))
pshc(5, 3) = pshc(5, 0) * 5
pshc(5, 4) = pshc(5, 0) * 4
pshc(6, 0) = x(0) / y(0) * x(1) / y(1)  *  x(2) / y(2) * x(3) / y(3) * x(4) / y(4) 
pshc(6, 3) = pshc(6, 0) * 5
pshc(6, 4) = pshc(6, 0) * 5
FOR z = 1 TO 6
    pshc(0, 3) = pshc(0, 3) + pshc(z, 3)
    pshc(0, 4) = pshc(0, 4) + pshc(z, 4)
NEXT z
REM PRINT pshc(0, 3); pshc(0, 4): SLEEP 2
RETURN
1111 ERASE pshc
REM calculates average number of post-split-hands and average number of pairs
REM that occur after the player finishes splitting pairs in a game wherein;
REM the rules allow the player to split to a maximum of two post-split hands.
FOR x = 0 TO 1
    x(x) = mc(pc1) - x
NEXT x
FOR y = 0 TO 1
    y(y) = mc(10) - y
NEXT y
pshc(1, 0) = (1 - x(0) / y(0)) * (1 - x(0) / y(1))
pshc(1, 1) = 2 * pshc(1, 0)
pshc(2, 0) = 2 * (1 - x(0) / y(0)) * x(0) / y(1)
pshc(2, 1) = 2 * pshc(2, 0)
pshc(2, 2) = pshc(2, 0) * 1
pshc(3, 0) = x(0) / y(0) * x(1) / y(1)
pshc(3, 1) = 2 * pshc(3, 0)
pshc(3, 2) = pshc(3, 0) * 2
FOR z = 1 TO 3
    pshc(0, 1) = pshc(0, 1) + pshc(z, 1)
    pshc(0, 2) = pshc(0, 2) + pshc(z, 2)
NEXT z
RETURN
1313 ERASE pshc
REM calculates average number of post-split-hands and average number of pairs
REM that occur after the player finishes splitting pairs in a game wherein;
REM the rules allow the player to split to a maximum of three post-split hands.
FOR x = 0 TO 3
    x(x) = mc(pc1) - x
NEXT x
FOR y = 0 TO 3
    y(y) = mc(10) - y
NEXT y
pshc(1, 0) = (1 - x(0) / y(0)) * (1 - x(0) / y(1))
pshc(1, 1) = 2 * pshc(1, 0)
pshc(2, 0) = 2 * (1 - x(0) / y(0)) * (1 - x(0) / y(1)) * (1 - x(0) / y(2)) * x(0) / y(3)
pshc(2, 1) = 3 * pshc(2, 0)
pshc(3, 0) = 5 * (1 - x(0) / y(0)) * (1 - x(0) / y(1)) * x(0) / y(2) * x(1) / y(3)
pshc(3, 1) = 3 * pshc(3, 0)
pshc(3, 2) = pshc(3, 0) * 1
pshc(4, 0) = 4 * (1 - x(0) / y(0)) * x(0) / y(1) * x(1) / y(2) * x(2) / y(3)
pshc(4, 1) = 3 * pshc(4, 0)
pshc(4, 2) = pshc(4, 0) * 2
pshc(5, 0) = x(0) / y(0) * x(1) / y(1) * x(2) / y(2) * x(3) / y(3)
pshc(5, 1) = 3 * pshc(5, 0)
pshc(5, 2) = pshc(5, 0) * 3
FOR z = 1 TO 5
    pshc(0, 1) = pshc(0, 1) + pshc(z, 1)
    pshc(0, 2) = pshc(0, 2) + pshc(z, 2)
NEXT z
RETURN
4343 ERASE pshc
REM calculates average number of post-split-hands and average number of pairs
REM that occur after the player finishes splitting pairs in a game wherein;
REM the rules allow the player to split to a maximum of four post-split hands.
FOR x = 0 TO 5
    x(x) = mc(pc1) - x
NEXT x
FOR y = 0 TO 5
    y(y) = mc(10) - y
NEXT y
pshc(1, 0) = (1 - x(0) / y(0)) * (1 - x(0) / y(1))
pshc(1, 1) = 2 * pshc(1, 0)
pshc(2, 0) = 2 * (1 - x(0) / y(0)) * (1 - x(0) / y(1)) * (1 - x(0) / y(2)) * x(0) / y(3)
pshc(2, 1) = 3 * pshc(2, 0)
pshc(3, 0) = 5 * (1 - x(0) / y(0)) * (1 - x(0) / y(1)) * (1 - x(0) / y(2))
pshc(3, 0) = pshc(3, 0) * (1 - x(0) / y(3)) * x(0) / y(4) * x(1) / y(5)
pshc(3, 1) = 4 * pshc(3, 0)
pshc(4, 0) = 14 * (1 - x(0) / y(0)) * (1 - x(0) / y(1)) * (1 - x(0) / y(2))
pshc(4, 0) = pshc(4, 0) * x(0) / y(3) * x(1) / y(4) * x(2) / y(5)
pshc(4, 1) = 4 * pshc(4, 0)
pshc(4, 2) = pshc(4, 0) * 1
pshc(5, 0) = 14 * (1 - x(0) / y(0)) * (1 - x(0) / y(1)) * x(0) / y(2)
pshc(5, 0) = pshc(5, 0) * x(1) / y(3) * x(2) / y(4) * x(3) / y(5)
pshc(5, 1) = 4 * pshc(5, 0)
pshc(5, 2) = pshc(5, 0) * 2
pshc(6, 0) = 6 * (1 - x(0) / y(0)) * x(0) / y(1) * x(1) / y(2) * x(2) / y(3)
pshc(6, 0) = pshc(6, 0) * x(3) / y(4) * x(4) / y(5)
pshc(6, 1) = 4 * pshc(6, 0)
pshc(6, 2) = pshc(6, 0) * 3
pshc(7, 0) = x(0) / y(0) * x(1) / y(1) * x(2) / y(2) * x(3) / y(3) * x(4) / y(4) * x(5) / y(5)
pshc(7, 1) = 4 * pshc(7, 0)
pshc(7, 2) = pshc(7, 0) * 4
FOR z = 1 TO 7
    pshc(0, 0) = pshc(0, 0) + pshc(z, 0)
    pshc(0, 1) = pshc(0, 1) + pshc(z, 1)
    pshc(0, 2) = pshc(0, 2) + pshc(z, 2)
    REM PRINT pshc(0, 0); pshc(0, 1); pshc(0, 2) / pshc(0, 1): SLEEP 2
NEXT z
RETURN
226 CLS: PRINT "BJ282.BAS asks the user to enter the folder/filename prefix"
PRINT "of the files to be used to store application output."
PRINT "BJ282.BAS asks users to defined the rules of a blackjack game."
PRINT "BJ282.BAS conducts a math analysis of the game by means of the"
PRINT "complete-combinatorial-analysis method. BJ282.BAS assembles"
PRINT "the results of the math analysis into a set of tables."
PRINT "The set of tables consists of a set of combination-dependent-"
PRINT "playing-strategy tables, and a combination-dependent-expected-"
PRINT "value table. BJ282 calculates an estimate of the expected value"
PRINT "of the game and adds it to the bottom of the combination-"
PRINT "dependent-playing-strategy table. BJ282.BAS stores the results"
PRINT "of the math analysis and the set of table in a set of files."
PRINT "A subset of the set of files are text files. The text files"
PRINT "contain the set of tables assembled by BJ282.BAS."
PRINT "A subset of the set of files that can be loaded into and used"
PRINT "by ULTIMA283.BAS to conduct a game simulation wherein; the"
PRINT "computer-controlled player uses the strategy tables to make"
PRINT "decisions on how to play the player's hand. A subset of the"
PRINT "set of files can be loaded into and used by ULTIMA293.BAS"
PRINT "to assemble a total-dependent-basic-strategy table."
PRINT " "
o$ = "     12/A612/A712/A812/A913/A613/A713/A813/A914/A614/A714/A814/A915/A615/A715/A815/A916/A616/A716/A816/A917/A617/A717/A817/A918/A718/A818/A919/A819/A9SplitSrndrDDown"
n$ = "     12   12   12   12   13   13   13   13   14   14   14   14   15   15   15   15   16   16   16   16   17   17   17   17   18   18   18   19   19   SplitSrndrDDown"
999 PRINT "enter the folder/filename prefix of the files to be used":
PRINT "to store application output": INPUT files$
PRINT " "
PRINT "press 'y' and enter for a standard deck composition";: INPUT ans$
v$ = files$ + "i.txt"
OPEN v$ FOR OUTPUT AS #1
PRINT #1, "BJ282.BAS asks the user to enter the folder/filename prefix"
PRINT #1, "of the files to be used to store application output."
PRINT #1, "BJ282.BAS asks users to defined the rules of a blackjack game."
PRINT #1, "BJ282.BAS conducts a math analysis of the game by means of the"
PRINT #1, "complete-combinatorial-analysis method. BJ282.BAS assembles"
PRINT #1, "the results of the math analysis into a set of tables."
PRINT #1, "The set of tables consists of a set of combination-dependent-"
PRINT #1, "playing-strategy tables, and a combination-dependent-expected-"
PRINT #1, "value table. BJ282 calculates an estimate of the expected value"
PRINT #1, "of the game and adds it to the bottom of the combination-"
PRINT #1, "dependent-playing-strategy table. BJ282.BAS stores the results"
PRINT #1, "of the math analysis and the set of table in a set of files."
PRINT #1, "A subset of the set of files are text files. The text files"
PRINT #1, "contain the set of tables assembled by BJ282.BAS."
PRINT #1, "A subset of the set of files that can be loaded into and used"
PRINT #1, "by ULTIMA283.BAS to conduct a game simulation wherein; the"
PRINT #1, "computer-controlled player uses the strategy tables to make"
PRINT #1, "decisions on how to play the player's hand. A subset of the"
PRINT #1, "set of files can be loaded into and used by ULTIMA293.BAS"
PRINT #1, "to assemble a total-dependent-basic-strategy table."
PRINT #1, " "
PRINT #1, "enter the folder/filename prefix of the files to be used":
PRINT #1, "to store application output": PRINT #1, files$
PRINT #1, " "
PRINT #1, "press 'y' and enter for a standard deck composition ";: PRINT #1, ans$
CLOSE #1
pdenom(0) = 65: pdenom(1) = 50: pdenom(2) = 51: pdenom(3) = 52: pdenom(4) = 53: pdenom(5) = 54: pdenom(6) = 55
pdenom(7) = 56: pdenom(8) = 57: pdenom(9) = 84: pdenom(10) = 74: pdenom(11) = 81: pdenom(12) = 75
IF ans$ <> "y" THEN
    PRINT "enter deck composition ";
    PRINT "as percentage or number of cards (p/n)";: INPUT sans$
    IF sans$ = "n" THEN
        FOR x = 0 TO 12
        PRINT "input the number of cards of denomination "; CHR$(pdenom(x)); " ";: INPUT a: card(x) = a: card(13) = card(13) + a: NEXT x
    END IF
    IF sans$ = "p" THEN
        FOR x = 0 TO 12
        PRINT "input the percentage of cards of denomination "; CHR$(pdenom(x)); " ";: INPUT a: card(x) = a * .52: card(13) = card(13) + a * .52: NEXT x
    END IF
END IF
IF ans$ = "y" THEN
    FOR x = 0 TO 12
    card(x) = 4: NEXT x: card(13) = 52
END IF
IF ans$ <> "y" THEN
    OPEN v$ FOR APPEND AS #1
    PRINT #1, "enter deck composition ";
    PRINT #1, "as percentage or number of cards (p/n)";: PRINT #1, sans$
    IF sans$ = "n" THEN
        FOR x = 0 TO 12
        PRINT #1, "input the number of cards of denomination "; CHR$(pdenom(x)); " ";: PRINT #1, card(x): NEXT x
    END IF
    IF sans$ = "p" THEN
        FOR x = 0 TO 12
        PRINT #1, "input the percentage of cards of denomination "; CHR$(pdenom(x)); " ";: PRINT #1, card(x): NEXT x
    END IF
    CLOSE #1
END IF
PRINT "input the number of decks used";: INPUT deck%
PRINT "what is the percentage of the shoe remaining at point of reshuffle.(15-100)";: INPUT shf
PRINT "input dealer soft finish";: INPUT j: j = j - 12
PRINT "press 1 and enter for dealer takes no hole card";: INPUT hole%: IF hole% <> 1 THEN hole% = 0
PRINT "press 1 and enter for player hands of 21 push against all dealer bjs";: INPUT push%
IF push% = 1 THEN ten% = 1 ELSE PRINT "press 1 and enter for player 21 pushes against dealer 10 up bjs only";: INPUT ten%
PRINT "press 1 and enter for automatic six card player hand win";: INPUT schw%
PRINT "press 1 and enter for insurance";: INPUT ins%
PRINT "press 1 and enter for surrender";: INPUT ls%
IF ls% = 1 THEN PRINT "press 1 and enter for early surrender";: INPUT early%
PRINT "press 1 and enter for split option";: INPUT splt%
IF splt% = 1 THEN
    PRINT "press 1 and enter for double down after split";: INPUT das%
    PRINT "what is the maximum number of split hands allowed";: INPUT most%
    IF most% > 2 THEN PRINT "press 1 and enter for resplit of aces is allowed";: INPUT rsa%
END IF
PRINT "press 1 and enter to restrict use of double down to specific hand totals";: INPUT dh%
IF dh% = 1 THEN PRINT "enter 9 for 9/10/11 only, enter 10 for 10/11 only, enter 11 for 11 only ";: INPUT ld%
IF NOT dh% = 1 THEN ld% = 1
PRINT "press 1 and enter to draw to split aces";: INPUT dsa%
PRINT "input 2 for blackjack pays 2:1 or 1.5 for pays 3:2 or 1 for pays 1:1";: INPUT blackjack
PRINT "press 1 and enter for blackjack pays off on split hands";: INPUT bjsh%
PRINT "press 1 and enter for all player hands of 21 pay off immediately";: INPUT immed%
PRINT "press 1 and enter for player blackjack beats dealer blackjack";: INPUT pbd%
PRINT "press 1 and enter for bonus 21 payoffs";: INPUT b21po%
IF b21po% = 1 THEN
    FOR x = 5 TO 8
        PRINT "enter payoff for"; x; " card 21";: INPUT mc21po(x - 1)
    NEXT x
    PRINT "enter payoff for 678 mixed suits";: INPUT ms678
    PRINT "enter payoff for 678 same suit";: INPUT ss678
    PRINT "enter payoff for 678 spades suit";: INPUT sp678
    PRINT "enter payoff for 777 mixed suits";: INPUT ms777
    IF deck% > 2 THEN
        PRINT "enter payoff for 777 same suit";: INPUT ss777
        PRINT "enter payoff for 777 spades suit";: INPUT sp777
        PRINT "enter jackpot for for 777 same suit against any dealer 7";: INPUT jp777
    END IF
END IF
IF b21po% <> 1 THEN
    FOR x = 5 TO 8
        mc21po(x - 1) = 0
    NEXT x
    ms678 = 0: ss678 = 0: sp678 = 0: ms777 = 0: ss777 = 0: sp777 = 0: jp777 = 0
END IF
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
hl% = 12: hu% = 19: sl% = 17: su% = 20
REM PRINT "input lower limit of player response strategies considered for hard hands"; : INPUT hl%
REM PRINT "input upper limit of player response strategies considered for hard hands"; : INPUT hu%
REM PRINT "input lower limit of player response strategies considered for soft hands"; : INPUT sl%
REM PRINT "input upper limit of player response strategies considered for soft hands"; : INPUT su%
PRINT "type and enter a brief description of rules": INPUT q$
OPEN v$ FOR APPEND AS #1
PRINT #1, "input the number of decks used ";: PRINT #1, deck%
PRINT #1, "what is the percentage of the shoe remaining at point of reshuffle.(15-100) ";: PRINT #1, shf
PRINT #1, "input dealer soft finish ";: PRINT #1, j + 12
PRINT #1, "press 1 and enter for dealer takes no hole card ";: PRINT #1, hole%
PRINT #1, "press 1 and enter for player hands of 21 push against all dealer bjs ";: PRINT #1, push%
IF push% = 1 THEN ten% = 1 ELSE PRINT #1, "press 1 and enter for player 21 pushes against dealer 10 up bjs only ";: PRINT #1, ten%
PRINT #1, "press 1 and enter for automatic six card player hand win ";: PRINT #1, schw%
PRINT #1, "press 1 and enter for insurance ";: PRINT #1, ins%
PRINT #1, "press 1 and enter for surrender ";: PRINT #1, ls%
IF ls% = 1 THEN PRINT #1, "press 1 and enter for early surrender ";: PRINT #1, early%
PRINT #1, "press 1 and enter for split option ";: PRINT #1, splt%
IF splt% = 1 THEN
    PRINT #1, "press 1 and enter for double down after split ";: PRINT #1, das%
    PRINT #1, "what is the maximum number of split hands allowed ";: PRINT #1, most%
    IF most% > 2 THEN PRINT #1, "press 1 and enter for resplit of aces is allowed ";: PRINT #1, rsa%
END IF
PRINT #1, "press 1 and enter to restrict use of double down to specific hand totals ";: PRINT #1, dh%
IF dh% = 1 THEN PRINT #1, "enter 9 for 9/10/11 only, enter 10 for 10/11 only, enter 11 for 11 only ";: PRINT #1, ld%
IF NOT dh% = 1 THEN ld% = 1
PRINT #1, "press 1 and enter to draw to split aces ";: PRINT #1, dsa%
PRINT #1, "input 2 for blackjack pays 2:1 or 1.5 for pays 3:2 or 1 for pays 1:1 ";: PRINT #1, blackjack
PRINT #1, "press 1 and enter for blackjack pays off on split hands ";: PRINT #1, bjsh%
PRINT #1, "press 1 and enter for all player hands of 21 pay off immediately ";: PRINT #1, immed%
PRINT #1, "press 1 and enter for player blackjack beats dealer blackjack ";: PRINT #1, pbd%
PRINT #1, "press 1 and enter for bonus 21 payoffs ";: PRINT #1, b21po%
IF b21po% = 1 THEN
    FOR x = 5 TO 7
        PRINT #1, "enter payoff for"; x; " card 21 ";: PRINT #1, mc21po(x)
    NEXT x
    PRINT #1, "enter payoff for 678 mixed suits ";: PRINT #1, ms678
    PRINT #1, "enter payoff for 678 same suit ";: PRINT #1, ss678
    PRINT #1, "enter payoff for 678 spades suit ";: PRINT #1, sp678
    PRINT #1, "enter payoff for 777 mixed suits ";: PRINT #1, ms777
    IF deck% > 2 THEN
        PRINT #1, "enter payoff for 777 same suit ";: PRINT #1, ss777
        PRINT #1, "enter payoff for 777 spades suit ";: PRINT #1, sp777
        PRINT #1, "enter jackpot for for 777 same suit against any dealer 7 ";: PRINT #1, jp777
    END IF
END IF
PRINT #1, "input lower limit of player response strategies considered for hard hands ";: PRINT #1, hl%
PRINT #1, "input upper limit of player response strategies considered for hard hands ";: PRINT #1, hu%
PRINT #1, "input lower limit of player response strategies considered for soft hands ";: PRINT #1, sl%
PRINT #1, "input upper limit of player response strategies considered for soft hands ";: PRINT #1, su%
PRINT #1, ""
PRINT #1, "type and enter a brief description of rules": PRINT #1, q$
PRINT #1, ""
PRINT #1, ""
PRINT #1, "START: "; DATE$; " "; TIME$
CLOSE #1
v$ = files$ + "ev.txt"
OPEN v$ FOR OUTPUT AS #1
PRINT #1, "the most favorable player responses": PRINT #1, q$
PRINT #1, ""
CLOSE #1
z = 0
FOR x = 12 TO 19
    FOR y = 5 TO 8
        IF x > y + 12 THEN y = x - 12
        z = z + 1
        g(z) = x: h(z) = y
NEXT y: NEXT x: g(30) = 1000: g(31) = 1000: h(30) = 1000: h(31) = 1000: h(32) = 1000: g(32) = 1000: ERASE df
FOR k = 9 TO 0 STEP -1
    FOR l = 9 TO k STEP -1
        FOR m = 9 TO 0 STEP -1
            n = k / 2 * (19 - k) + l
            pc1 = k: pc2 = l: dc1 = m
            ERASE mc
            FOR x = 0 TO 12
                IF x > 8 THEN mc(9) = mc(9) + card(x) * deck% ELSE mc(x) = card(x) * deck%
            NEXT x
            mc(10) = card(13) * deck%
            FOR x = 0 TO 32
            aq(n, dc1, x) = -1: NEXT x
            IF mc(pc1) < 1 THEN mc(19) = mc(pc1) ELSE mc(19) = 1
            IF mc(pc1) = 0 THEN GOTO 456 ELSE mc(pc1) = mc(pc1) - mc(19): mc(10) = mc(10) - mc(19)
            IF mc(pc2) < 1 THEN mc(19) = mc(pc2) ELSE mc(19) = 1
            IF mc(pc2) = 0 THEN GOTO 456 ELSE mc(pc2) = mc(pc2) - mc(19): mc(10) = mc(10) - mc(19)
            IF mc(dc1) < 1 THEN mc(19) = mc(dc1) ELSE mc(19) = 1
            IF mc(dc1) = 0 THEN GOTO 456 ELSE mc(dc1) = mc(dc1) - mc(19): mc(10) = mc(10) - mc(19)
            IF ins% = 1 THEN IF dc1 = 0 THEN IF 3 * mc(9) / mc(10) > 1 THEN insur = 3 * mc(9) / mc(10) - 1 ELSE insur = 0
            IF n = 9 THEN
                q# = blackjack
                IF pbd% <> 1 THEN
                    IF dc1 = 0 THEN q# = q# * (1 - mc(9) / mc(10))
                    IF dc1 = 9 THEN q# = q# * (1 - mc(0) / mc(10))
                END IF
                hit(n, dc1) = q#: br(n, dc1) = 1: aq(n, dc1, 1) = q#: GOTO 333
            END IF
            ERASE f: ERASE dbj
            FOR g = 12 TO 19
                FOR h = 5 TO 8
                    IF h + 12 < g THEN h = g - 12
                    I = I + 1
                    IF g >= hl% AND g <= hu% AND h + 12 >= sl% AND h + 12 <= su% THEN
                        mc0 = mc(0): mc9 = mc(9): mc10 = mc(10)
                        GOSUB 777
                        IF pc1 * pc2 = 0 THEN
                            IF pc1 + pc2 > 5 AND h < 6 THEN IF I < 25 THEN I = I + 1: h = h + 1
                            IF pc1 + pc2 > 6 AND h < 7 THEN IF I < 28 THEN I = I + 1: h = h + 1
                            IF pc1 + pc2 > 7 AND h < 8 THEN I = I + 1: h = h + 1
                        END IF
                        IF pc1 * pc2 <> 0 THEN
                            IF pc1 + pc2 > 4 AND h < 6 THEN IF I < 25 THEN I = I + 1: h = h + 1
                            IF pc1 + pc2 > 5 AND h < 7 THEN IF I < 28 THEN I = I + 1: h = h + 1
                            IF pc1 + pc2 > 6 AND h < 8 THEN I = I + 1: h = h + 1
                        END IF
                    END IF
            NEXT h: NEXT g: I = 0
            GOSUB 888
            IF dh% <> 1 THEN GOSUB 222
            IF dh% = 1 THEN IF (12 > pc1 + pc2 + 2 AND pc1 + pc2 + 2 > ld% - 1) THEN GOSUB 222
            IF ls% = 1 THEN GOSUB 234
            ERASE f: ERASE dbj
            IF (pc1 <> pc2 OR splt% <> 1) THEN GOTO 333 ELSE split% = 1
            pc3 = pc2
            FOR pc2 = 9 TO 0 STEP -1
                ERASE mc
                FOR x = 0 TO 12
                    IF x > 8 THEN mc(9) = mc(9) + card(x) * deck% ELSE mc(x) = card(x) * deck%
                NEXT x
                mc(10) = card(13) * deck%
                IF mc(pc1) < 1 THEN mc(19) = mc(pc1) ELSE mc(19) = 1
                IF mc(pc1) = 0 THEN GOTO 444 ELSE mc(pc1) = mc(pc1) - mc(19): mc(10) = mc(10) - mc(19)
                IF mc(pc1) < 1 THEN mc(19) = mc(pc1) ELSE mc(19) = 1
                IF mc(pc1) = 0 THEN GOTO 444 ELSE mc(pc1) = mc(pc1) - mc(19): mc(10) = mc(10) - mc(19)
                IF mc(dc1) < 1 THEN mc(19) = mc(dc1) ELSE mc(19) = 1
                IF mc(dc1) = 0 THEN GOTO 444 ELSE mc(dc1) = mc(dc1) - mc(19): mc(10) = mc(10) - mc(19)
                mc0 = mc(0): mc9 = mc(9): mc10 = mc(10)
                IF most% = 2 THEN GOSUB 1111
                IF most% = 3 THEN
                    IF pc1 <> 0 OR rsa% = 1 THEN
                        GOSUB 1313
                    END IF
                    IF pc1 = 0 AND rsa% <> 1 THEN
                        GOSUB 1111
                    END IF
                END IF
                IF most% > 3 THEN
                    IF pc1 <> 0 OR rsa% = 1 THEN
                        GOSUB 4343
                    END IF
                    IF pc1 = 0 AND rsa% <> 1 THEN
                        GOSUB 1111
                    END IF
                END IF
                pshc(0, 2) = pshc(0, 2) / pshc(0, 1)
                IF pc2 <> pc1 THEN prob# = mc(pc2) / mc(10) / (1 - ((mc(pc1) - pshc(0, 2) * mc(10)) / (mc(10) - pshc(0, 2) * mc(10))))
                IF pc2 = pc1 THEN prob# = pshc(0, 2)
                probs# = probs# + prob#: splitprob(pc2) = prob#
                IF mc(pc2) < 1 THEN mc(19) = mc(pc2) ELSE mc(19) = 1
                IF mc(pc2) = 0 THEN GOTO 444 ELSE mc(pc2) = mc(pc2) - mc(19): mc(10) = mc(10) - mc(19)
                mc0 = mc(0): mc9 = mc(9): mc10 = mc(10): IF pc1 = 0 AND dsa% <> 1 THEN nodsa% = 1
                IF most% = 2 THEN
                    GOSUB 404
                END IF
                IF most% = 3 THEN
                    IF pc1 <> 0 OR rsa% = 1 THEN
                        IF pc1 <> pc2 THEN 
                            GOSUB 505
                        END IF
                        IF pc1 = pc2 THEN
                            GOSUB 808
                        END IF
                    END IF
                    IF pc1 = 0 AND rsa% <> 1 THEN
                        GOSUB 404
                    END IF
                END IF
                IF most% > 3 THEN
                    IF pc1 <> 0 OR rsa% = 1 THEN
                        IF pc1 <> pc2 THEN
                            GOSUB 1414
                        END IF
                        IF pc1 = pc2 THEN
                            GOSUB 2929
                        END IF
                    END IF
                    IF pc1 = 0 AND rsa% <> 1 THEN
                        GOSUB 404
                    END IF
                END IF
                FOR x = 0 TO 9
                    IF pc1 = x THEN
                        IF mc(x) < pshc(0, 4) THEN mc(19) = mc(x) ELSE mc(19) = pshc(0, 4)
                        IF mc(x) = 0 THEN GOTO 444 ELSE mc(x) = mc(x) - mc(19): mc(10) = mc(10) - mc(19)
                    END IF
                    IF pc1 <> x THEN
                        IF mc(x) < (pshc(0, 3) - pshc(0, 4)) * mc(x) / mc(10) THEN mc(19) = mc(x) ELSE mc(19) = (pshc(0, 3) - pshc(0, 4)) * mc(x) / mc(10)
                        IF mc(x) = 0 THEN GOTO 444 ELSE mc(x) = mc(x) - mc(19): mc(10) = mc(10) - mc(19)
                    END IF
                NEXT x
                ERASE f: ERASE dbj
                FOR g = 12 TO 19
                    FOR h = 5 TO 8
                        IF h + 12 < g THEN h = g - 12
                        I = I + 1
                        IF g >= hl% AND g <= hu% AND h + 12 >= sl% AND h + 12 <= su% THEN
                            IF dsa% <> 1 AND pc1 = 0 THEN nodsa% = 1
                            GOSUB 777
                            nodsa% = 0
                            IF pc1 * pc2 = 0 THEN
                                IF pc1 + pc2 > 5 AND h < 6 THEN IF I < 25 THEN I = I + 1: h = h + 1
                                IF pc1 + pc2 > 6 AND h < 7 THEN IF I < 28 THEN I = I + 1: h = h + 1
                                IF pc1 + pc2 > 7 AND h < 8 THEN I = I + 1: h = h + 1
                            END IF
                            IF pc1 * pc2 <> 0 THEN
                                IF pc1 + pc2 > 4 AND h < 6 THEN IF I < 25 THEN I = I + 1: h = h + 1
                                IF pc1 + pc2 > 5 AND h < 7 THEN IF I < 28 THEN I = I + 1: h = h + 1
                                IF pc1 + pc2 > 6 AND h < 8 THEN I = I + 1: h = h + 1
                            END IF
                        END IF
                NEXT h: NEXT g: I = 0
                GOSUB 888
                IF das% = 1 THEN
                    IF dsa% = 1 OR pc1 <> 0 THEN
                        IF dh% <> 1 THEN GOSUB 222
                        IF dh% = 1 THEN IF (12 > pc1 + pc2 + 2 AND pc1 + pc2 + 2 > ld% - 1) THEN GOSUB 222
                    END IF
                END IF
                ERASE f: ERASE dbj
            444 NEXT pc2: pc2 = pc3: q# = 0: qdbj# = 0: IF pc1 <> 0 OR dsa% = 1 THEN mc(pc2) = mc(pc2) + mc(19): mc(10) = mc(10) + mc(19)
            FOR x = 0 TO 9
                q# = q# + splithands(x) * splitprob(x)
                qdbj# = qdbj# + splitdbj(x) * splitprob(x)
                'IF dc1 = 0 OR dc1 = 9 THEN
                '    OPEN v$ FOR APPEND AS #1
                '    PRINT #1, "pc2 = ";: PRINT #1, x + 1;: PRINT #1, " splithands(x) = ";: PRINT #1, USING "#.######"; splithands(x);
                '    PRINT #1, " splitdbj(x) = ";: PRINT #1, USING "#.######"; splitdbj(x);
                '    PRINT #1, " splitprob(x) = ";: PRINT #1, USING "#.######"; splitprob(x)
                '    IF pc1 <= x THEN n = pc1 / 2 * (19 - pc1) + x ELSE n = x / 2 * (19 - x) + pc1
                '    PRINT #1, "far(n, dc1) = ";: PRINT #1, MID$(o$, far(n, dc1) * 5 + 1, 5);
                '    PRINT #1, " sar(n, dc1) = ";: PRINT #1, MID$(o$, sar(n, dc1) * 5 + 1, 5)
                '    CLOSE #1
                'END IF
            NEXT x
            IF pc1 <= pc2 THEN n = pc1 / 2 * (19 - pc1) + pc2 ELSE n = pc2 / 2 * (19 - pc2) + pc1
            'IF dc1 = 0 OR dc1 = 9 THEN
            '    OPEN v$ FOR APPEND AS #1
            '    PRINT #1, "q# = ";: PRINT #1, USING "#.######"; q#;
            '    PRINT #1, " qdbj# = ";: PRINT #1, USING "#.######"; qdbj#;
            '    PRINT #1, " probs# = ";: PRINT #1, USING "#.######"; probs#
            '    CLOSE #1
            'END IF
            ERASE splithands: ERASE splitdbj: ERASE splitprob: probs# = 0
            IF hole% <> 1 THEN q# = pshc(0, 1) * q# + qdbj#
            IF hole% = 1 THEN q# = pshc(0, 1) * q#
            split = q#: aq(n, dc1, 30) = q#
            IF q# > hit(pc1 * (21 - pc1) / 2, dc1) THEN hit(pc1 * (21 - pc1) / 2, dc1) = q#: br(pc1 * (21 - pc1) / 2, dc1) = 30
            333 ERASE mc
            FOR x = 0 TO 12
                IF x > 8 THEN mc(9) = mc(9) + card(x) * deck% ELSE mc(x) = card(x) * deck%
            NEXT x
            mc(10) = card(13) * deck%
            IF mc(pc1) < 1 THEN mc(19) = mc(pc1) ELSE mc(19) = 1
            IF mc(pc1) = 0 THEN GOTO 456 ELSE mc(pc1) = mc(pc1) - mc(19): mc(10) = mc(10) - mc(19)
            IF mc(pc2) < 1 THEN mc(19) = mc(pc2) ELSE mc(19) = 1
            IF mc(pc2) = 0 THEN GOTO 456 ELSE mc(pc2) = mc(pc2) - mc(19): mc(10) = mc(10) - mc(19)
            IF mc(dc1) < 1 THEN mc(19) = mc(dc1) ELSE mc(19) = 1
            IF mc(dc1) = 0 THEN GOTO 456 ELSE mc(dc1) = mc(dc1) - mc(19): mc(10) = mc(10) - mc(19)
            mc0 = mc(0): mc9 = mc(9): mc10 = mc(10)
            IF ins% = 1 THEN IF dc1 = 0 THEN IF 3 * mc(9) / mc(10) > 1 THEN insur = 3 * mc(9) / mc(10) - 1 ELSE insur = 0
            hit(n, dc1) = hit(n, dc1) + insur
            GOSUB 555
            CLS
            PRINT "               player cards are"; pc1 + 1; "and"; pc2 + 1; "dealer card is"; dc1 + 1;
            PRINT "                  given the user input deck composition,"
            PRINT "                       here are the dealer finish,"
            PRINT "     and player win minus loss expectations for best stand strategy,"
            PRINT "  double down, surrender, split and insurance options where applicable."
            PRINT " "
            PRINT "     ";
            FOR x = 1 TO 5
                PRINT x + 16; "    ";
            NEXT x
            PRINT " bust  ";
            IF dc1 = 0 OR dc1 = 9 THEN
                PRINT "  AJ   ";
            END IF
            PRINT " "
            account# = account# + df(11)
            PRINT USING "####"; dc1 + 1;
            FOR y = 0 TO 5
                IF y = 5 THEN y = 12
                PRINT USING "####.###"; df(y) / (df(11) + 1E-08) * 100;
                IF y = 12 THEN
                    IF dc1 = 0 OR dc1 = 9 THEN
                        PRINT USING "####.###"; df(10) / (df(11) + 1E-08) * 100;
                    END IF
                END IF
            NEXT y: PRINT " "
            PRINT "total hands accounted for are ";: PRINT USING "###.###"; account# * 100;: PRINT "% of all possible"
            IF n = 9 THEN PRINT "blackjack yields a win minus loss expectation of";: PRINT USING "##.####"; hit(n, dc1)
            IF n <> 9 THEN
                PRINT MID$(o$, standr% * 5 + 1, 5);: PRINT " stand strategy yields a win minus loss expectation of";: PRINT USING "##.####"; stand
                IF dh% <> 1 THEN PRINT "double down strategy yields a win minus loss expectation of";: PRINT USING "##.####"; ddown
                IF dh% = 1 THEN IF (12 > pc1 + pc2 + 2 AND pc1 + pc2 + 2 > ld% - 1) THEN PRINT "double down strategy yields a win minus loss expectation of";: PRINT USING "##.####"; ddown
                IF ls% = 1 THEN IF early% <> 1 THEN PRINT "L surrender strategy yields a win minus loss expectation of";: PRINT USING "##.####"; srndr
                IF ls% = 1 THEN IF early% = 1 THEN PRINT "E surrender strategy yields a win minus loss expectation of";: PRINT USING "##.####"; srndr
                IF splt% = 1 THEN IF pc1 = pc2 THEN PRINT USING "##"; pc1 + 1;: PRINT " ";: PRINT USING "##"; pc1 + 1;: PRINT " split strategy yields a win minus loss expectation of";: PRINT USING "##.####"; split
                IF ins% = 1 THEN IF dc1 = 0 THEN IF insur > 0 THEN PRINT "insurance on strategy yields a win minus loss strategy of";: PRINT USING "##.####"; insur
                PRINT "": account# = 0
                IF br(n, dc1) < 30 THEN PRINT MID$(o$, br(n, dc1) * 5 + 1, 5); " stand is found to be the optimal strategy. "
                IF br(n, dc1) = 30 THEN PRINT USING "##"; pc1 + 1;: PRINT " ";: PRINT USING "##"; pc1 + 1;: PRINT " split is found to be the optimal strategy. "
                IF br(n, dc1) = 31 THEN IF early% <> 1 THEN PRINT "L surrender is found to be the optimal strategy. "
                IF br(n, dc1) = 31 THEN IF early% = 1 THEN PRINT "E surrender is found to be the optimal strategy. "
                IF br(n, dc1) = 32 THEN PRINT "double down is found to be the optimal strategy. "
            END IF
            OPEN v$ FOR APPEND AS #1
            account# = 0
            PRINT #1, ""
            PRINT #1, "               player cards are"; pc1 + 1; "and"; pc2 + 1; "dealer card is"; dc1 + 1
            PRINT #1, "                  given the user input deck composition,"
            PRINT #1, "                       here are the dealer finish,"
            PRINT #1, "     and player win minus loss expectations for best stand strategy,"
            PRINT #1, "  double down, surrender, split and insurance options where applicable."
            PRINT #1, " "
            PRINT #1, "     ";
            FOR x = 1 TO 5
                PRINT #1, x + 16; "    ";
            NEXT x
            PRINT #1, " bust  ";
            IF dc1 = 0 OR dc1 = 9 THEN
                PRINT #1, "  AJ   ";
            END IF
            PRINT #1, " "
            account# = account# + df(11)
            PRINT #1, USING "####"; dc1 + 1;
            FOR y = 0 TO 5
                IF y = 5 THEN y = 12
                PRINT #1, USING "####.###"; df(y) / (df(11) + 1E-08) * 100;
                IF y = 12 THEN
                    IF dc1 = 0 OR dc1 = 9 THEN
                        PRINT #1, USING "####.###"; df(10) / (df(11) + 1E-08) * 100;
                    END IF
                END IF
            NEXT y: PRINT #1, " "
            PRINT #1, "total hands accounted for are ";: PRINT #1, USING "###.###"; account# * 100;: PRINT #1, "% of all possible"
            IF n = 9 THEN PRINT #1, "blackjack yields a win minus loss expectation of";: PRINT #1, USING "##.####"; hit(n, dc1)
            IF n <> 9 THEN
                PRINT #1, MID$(o$, standr% * 5 + 1, 5);: PRINT #1, " stand strategy yields a win minus loss expectation of";: PRINT #1, USING "##.####"; stand
                IF dh% <> 1 THEN PRINT #1, "double down strategy yields a win minus loss expectation of";: PRINT #1, USING "##.####"; ddown
                IF dh% = 1 THEN IF (12 > pc1 + pc2 + 2 AND pc1 + pc2 + 2 > ld% - 1) THEN PRINT #1, "double down strategy yields a win minus loss expectation of";: PRINT #1, USING "##.####"; ddown
                IF ls% = 1 THEN IF early% <> 1 THEN PRINT #1, "L surrender strategy yields a win minus loss expectation of";: PRINT #1, USING "##.####"; srndr
                IF ls% = 1 THEN IF early% = 1 THEN PRINT #1, "E surrender strategy yields a win minus loss expectation of";: PRINT #1, USING "##.####"; srndr
                IF splt% = 1 THEN IF pc1 = pc2 THEN PRINT #1, USING "##"; pc1 + 1;: PRINT #1, " ";: PRINT #1, USING "##"; pc1 + 1;: PRINT #1, " split strategy yields a win minus loss expectation of";: PRINT #1, USING "##.####"; split
                IF ins% = 1 THEN IF dc1 = 0 THEN IF insur > 0 THEN PRINT #1, "insurance on strategy yields a win minus loss strategy of";: PRINT #1, USING "##.####"; insur
                PRINT #1, "": account# = 0
                IF br(n, dc1) < 30 THEN PRINT #1, MID$(o$, br(n, dc1) * 5 + 1, 5); " stand is found to be the optimal strategy. "
                IF br(n, dc1) = 30 THEN PRINT #1, USING "##"; pc1 + 1;: PRINT #1, " ";: PRINT #1, USING "##"; pc1 + 1;: PRINT #1, " split is found to be the optimal strategy. "
                IF br(n, dc1) = 31 THEN IF early% <> 1 THEN PRINT #1, "L surrender is found to be the optimal strategy. "
                IF br(n, dc1) = 31 THEN IF early% = 1 THEN PRINT #1, "E surrender is found to be the optimal strategy. "
                IF br(n, dc1) = 32 THEN PRINT #1, "double down is found to be the optimal strategy. "
            END IF
            CLOSE #1
            ERASE df
456 split% = 0: insur = 0: NEXT m: NEXT l: NEXT k
total# = 0
FOR k = 0 TO 9
    FOR l = k TO 9
        n = k / 2 * (19 - k) + l
        FOR m = 0 TO 9
            pc1 = k: pc2 = l: dc1 = m
            ERASE mc
            FOR x = 0 TO 12
                IF x > 8 THEN mc(9) = mc(9) + card(x) * deck% ELSE mc(x) = card(x) * deck%
            NEXT x
            mc(10) = card(13) * deck%
            IF mc(pc1) < 1 THEN mc(19) = mc(pc1) ELSE mc(19) = 1
            IF mc(pc1) = 0 THEN GOTO 567 ELSE prob# = mc(pc1) / mc(10): mc(pc1) = mc(pc1) - mc(19): mc(10) = mc(10) - mc(19)
            IF mc(pc2) < 1 THEN mc(19) = mc(pc2) ELSE mc(19) = 1
            IF mc(pc2) = 0 THEN GOTO 567 ELSE prob# = prob# * mc(pc2) / mc(10): mc(pc2) = mc(pc2) - mc(19): mc(10) = mc(10) - mc(19)
            IF pc1 <> pc2 THEN prob# = prob# * 2
            puc(n) = prob#
            IF mc(dc1) < 1 THEN mc(19) = mc(dc1) ELSE mc(19) = 1
            IF mc(dc1) = 0 THEN GOTO 567 ELSE prob# = prob# * mc(dc1) / mc(10): mc(dc1) = mc(dc1) - mc(19): mc(10) = mc(10) - mc(19)
            total# = total# + hit(n, dc1) * prob#: probs# = probs# + prob#
567 NEXT m: NEXT l: NEXT k
ERASE mc
FOR x = 0 TO 12
    IF x > 8 THEN mc(9) = mc(9) + card(x) * deck% ELSE mc(x) = card(x) * deck%
NEXT x
mc(10) = card(13) * deck%
FOR x = 0 TO 9
duc(x) = mc(x) / mc(10): NEXT x
v$ = files$ + "a.dat"
OPEN v$ FOR RANDOM AS #1 LEN = LEN(sd)
sd.c1 = card(0): sd.c2 = card(1): sd.c3 = card(2): sd.c4 = card(3): sd.c5 = card(4)
sd.c6 = card(5): sd.c7 = card(6): sd.c8 = card(7): sd.c9 = card(8): sd.cT = card(9)
sd.cJ = card(10): sd.cQ = card(11): sd.cK = card(12): sd.dwt = dwt%: sd.dbc = dbc%
sd.ls = ls%: sd.early = early%: sd.j = j: sd.hole = hole%: sd.push = push%: sd.ten = ten%
sd.schw = schw%: sd.splt = splt%: sd.nsh = most%: sd.rsa = rsa%: sd.das = das%
sd.dh = dh%: sd.ld = ld%: sd.blackjack = blackjack: sd.ins = ins%: sd.dsa = dsa%
sd.bjsh = bjsh%: sd.deck = deck%: sd.shf = shf: sd.immed = immed%: sd.pbd = pbd%
sd.b21po = b21po%: sd.ms777 = ms777: sd.ss777 = ss777: sd.sp777 = sp777: sd.jp777 = jp777
sd.po5c21 = mc21po(4): sd.po6c21 = mc21po(5): sd.po7c21 = mc21po(6): sd.po8c21 = mc21po(7)
sd.ms678 = ms678: sd.ss678 = ss678: sd.sp678 = sp678
IF LEN(q$) > 80 THEN sd.description = MID$(q$, 1, 80) ELSE sd.description = q$
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
v$ = files$ + "t.dat"
w = 0
OPEN v$ FOR RANDOM AS #1 LEN = LEN(database)
FOR x = 0 TO 54
    FOR y = 0 TO 9
        FOR z = 0 TO 32
            w = w + 1
            database.dat = aq(x, y, z)
            PUT #1, w, database
NEXT z: NEXT y: NEXT x
CLOSE #1
v$ = files$ + "i.txt"
OPEN v$ FOR APPEND AS #1
PRINT #1, ""
PRINT #1, "END:   "; DATE$; " "; TIME$
CLOSE #1
v$ = files$ + "o.txt"
OPEN v$ FOR OUTPUT AS #1
321 PRINT #1, "the most favorable player responses": PRINT #1, q$
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
PRINT #1, "win minus loss margins for two card combos verses dealer up cards "
FOR x = 0 TO 10
    IF x = 0 THEN PRINT #1, "     ";
    IF x > 0 THEN PRINT #1, USING "##"; x;: PRINT #1, "     ";
NEXT x
PRINT #1, " "
FOR x = 0 TO 10
    IF x = 0 THEN PRINT #1, "   ";
    IF x > 0 THEN PRINT #1, USING "###.##"; duc(x - 1) * 100;: PRINT #1, " ";
NEXT x
PRINT #1, "    %"
FOR x = 0 TO 54
    FOR y = 0 TO 10
        IF y = 0 THEN PRINT #1, MID$(b$, x * 2 + 1, 2); " ";
        IF y > 0 THEN PRINT #1, USING "###.##"; hit(x, y - 1) * 100;: PRINT #1, " ";
NEXT y: PRINT #1, USING "##.##"; puc(x) * 100: NEXT x
PRINT #1, " "
PRINT #1, "total win minus loss margin is ";: PRINT #1, USING "##.###"; total# * 100;: PRINT #1, "%"
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

