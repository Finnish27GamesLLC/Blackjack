'ULTIMA282.BAS is a software application that enables the user to
'program their computer to conduct abbreviated-combinatorial analyses
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

_TITLE "ULTIMA282.BAS"
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
DIM database AS xbase: DIM num AS bs: DIM g(32) AS INTEGER: DIM h(32) AS INTEGER: DIM splithands(10, 9)
DIM sd AS bsrules: DIM hit(54, 9): DIM sar(54, 9) AS INTEGER: DIM dd(3 TO 7) AS INTEGER: DIM prob(54, 9)
DIM far(54, 9) AS INTEGER: DIM br(54, 9) AS INTEGER: DIM dmult(8) AS INTEGER: DIM pdenom(12) AS INTEGER
DIM dadd(8) AS INTEGER: DIM dp(8): DIM duc(10): DIM f(29, 24): DIM card(22) AS INTEGER: DIM aq(54, 9, 32)
DIM pfar(54, 9, 32, 24): DIM dfar(54, 9, 12): DIM psar(54, 9, 32, 24): DIM dsar(54, 9, 12): DIM dbr(54, 9, 12): DIM pbr(54, 9, 32, 24)
DIM mc(21): DIM df(12): DIM mv(5) AS INTEGER: DIM puc(54): DIM mc21po(11) AS SINGLE: DIM pshc(7, 2) AS DOUBLE
DIM pc1 AS INTEGER: DIM pc2 AS INTEGER: DIM dc1 AS INTEGER: DIM x(5) AS DOUBLE: DIM y(5) AS DOUBLE
GOTO 226
777 dmult(2) = pc1 * pc2: dadd(2) = pc1 + pc2: dp(2) = 1
IF bjsh% = 1 THEN IF dadd(2) = 19 THEN f(I, 17) = f(I, 17) + dp(2): GOTO 18
IF dadd(2) > g - 3 THEN mv(1) = dadd(2) - 10: f(I, mv(1)) = f(I, mv(1)) + dp(2): GOTO 18
IF dmult(2) = 0 AND dadd(2) > h - 1 THEN mv(1) = dadd(2): f(I, mv(1)) = f(I, mv(1)) + dp(2): GOTO 18
FOR s = 0 TO 9 STEP 1
    dmult(3) = dmult(2) * s: dadd(3) = dadd(2) + s
    IF mc(s) < 1 THEN mc(14) = mc(s) ELSE mc(14) = 1
    IF mc(s) = 0 THEN GOTO 19 ELSE dp(3) = dp(2) * mc(s) / mc(10): mc(s) = mc(s) - mc(14): mc(10) = mc(10) - mc(14)
    IF b21po% = 1 THEN
        IF dadd(3) = 18 THEN
            IF pc1 = 6 AND pc2 = 6 THEN
                IF dc1 = 6 THEN po777 = mc21po(1) + mc21po(11) ELSE po777 = mc21po(1) + mc21po(2) + mc21po(3)
                IF po777 <> 0 THEN f(I, 18) = f(I, 18) + dp(3): mc(s) = mc(s) + mc(14): mc(10) = mc(10) + mc(14): GOTO 19
            END IF
            IF pc1 = 5 THEN IF pc2 = 6 OR pc2 = 7 THEN po678 = mc21po(8) + mc21po(9) + mc21po(10): IF po678 <> 0 THEN f(I, 19) = f(I, 19) + dp(3): mc(s) = mc(s) + mc(14): mc(10) = mc(10) + mc(14): GOTO 19
            IF pc1 = 6 THEN IF pc2 = 5 OR pc2 = 7 THEN po678 = mc21po(8) + mc21po(9) + mc21po(10): IF po678 <> 0 THEN f(I, 19) = f(I, 19) + dp(3): mc(s) = mc(s) + mc(14): mc(10) = mc(10) + mc(14): GOTO 19
            IF pc1 = 7 THEN IF pc2 = 5 OR pc2 = 6 THEN po678 = mc21po(8) + mc21po(9) + mc21po(10): IF po678 <> 0 THEN f(I, 19) = f(I, 19) + dp(3): mc(s) = mc(s) + mc(14): mc(10) = mc(10) + mc(14): GOTO 19
        END IF
    END IF
    IF dadd(3) > g - 4 THEN mv(1) = dadd(3) - 9: f(I, mv(1)) = f(I, mv(1)) + dp(3): mc(s) = mc(s) + mc(14): mc(10) = mc(10) + mc(14): GOTO 19
    IF dmult(3) = 0 AND dadd(3) > h - 2 AND dadd(3) < 9 THEN mv(1) = dadd(3) + 1: f(I, mv(1)) = f(I, mv(1)) + dp(3): mc(s) = mc(s) + mc(14): mc(10) = mc(10) + mc(14): GOTO 19
    IF dd(3) = 1 AND dadd(3) + 1 - k < 10 THEN
        IF split% <> 1 THEN ddn = k / 2 * (19 - k) + dadd(3) + 1 - k: IF br(ddn, m) = 32 THEN dd% = 2 ELSE dd% = 1
        IF split% = 1 AND das% = 1 THEN
            IF pc1 <= pc2 THEN ddn = pc1 / 2 * (19 - pc1) + dadd(3) + 1 - pc1: IF sar(ddn, m) = 32 THEN dd% = 2 ELSE dd% = 1
            IF pc2 < pc1 THEN ddn = pc2 / 2 * (19 - pc2) + dadd(3) + 1 - pc2: IF far(ddn, m) = 32 THEN dd% = 2 ELSE dd% = 1
        END IF
        IF split% = 1 AND das% <> 1 THEN dd% = 1
    END IF
    FOR t = 0 TO 9 STEP 1
        dmult(4) = dmult(3) * t: dadd(4) = dadd(3) + t
        IF mc(t) < 1 THEN mc(15) = mc(t) ELSE mc(15) = 1
        IF mc(t) = 0 THEN GOTO 20 ELSE dp(4) = dp(3) * mc(t) / mc(10): mc(t) = mc(t) - mc(15): mc(10) = mc(10) - mc(15)
        IF dadd(4) > g - 5 THEN mv(1) = dadd(4) - 8: f(I, mv(1)) = f(I, mv(1)) + dp(4) * dd%: mc(t) = mc(t) + mc(15): mc(10) = mc(10) + mc(15): GOTO 20
        IF dmult(4) = 0 AND dadd(4) > h - 3 AND dadd(4) < 8 THEN mv(1) = dadd(4) + 2: f(I, mv(1)) = f(I, mv(1)) + dp(4) * dd%: mc(t) = mc(t) + mc(15): mc(10) = mc(10) + mc(15): GOTO 20
        IF dd% = 2 THEN
            IF dmult(4) = 0 AND dadd(4) < 8 THEN mv(1) = dadd(4) + 2 ELSE mv(1) = dadd(4) - 8
            IF mv(1) < 0 THEN mv(1) = 0
            f(I, mv(1)) = f(I, mv(1)) + dp(4) * dd%
            mc(t) = mc(t) + mc(15)
            mc(10) = mc(10) + mc(15)
            GOTO 20
        END IF
        IF dd(4) = 1 AND dadd(4) + 2 - k < 10 THEN
            IF split% <> 1 THEN ddn = k / 2 * (19 - k) + dadd(4) + 2 - k: IF br(ddn, m) = 32 THEN dd% = 2 ELSE dd% = 1
            IF split% = 1 AND das% = 1 THEN
                IF pc1 <= pc2 THEN ddn = pc1 / 2 * (19 - pc1) + dadd(4) + 2 - pc1: IF sar(ddn, m) = 32 THEN dd% = 2 ELSE dd% = 1
                IF pc2 < pc1 THEN ddn = pc2 / 2 * (19 - pc2) + dadd(4) + 2 - pc2: IF far(ddn, m) = 32 THEN dd% = 2 ELSE dd% = 1
            END IF
            IF split% = 1 AND das% <> 1 THEN dd% = 1
        END IF
        FOR u = 0 TO 9 STEP 1
            dmult(5) = dmult(4) * u: dadd(5) = dadd(4) + u
            IF mc(u) < 1 THEN mc(16) = mc(u) ELSE mc(16) = 1
            IF mc(u) = 0 THEN GOTO 21 ELSE dp(5) = dp(4) * mc(u) / mc(10): mc(u) = mc(u) - mc(16): mc(10) = mc(10) - mc(16)
            IF dadd(5) > g - 6 THEN
                mv(1) = dadd(5) - 7
                IF b21po% = 1 AND mv(1) = 9 THEN IF mc21po(4) <> 0 THEN f(I, 20) = f(I, 20) + dp(5) * dd%: mc(u) = mc(u) + mc(16): mc(10) = mc(10) + mc(16): GOTO 21
                f(I, mv(1)) = f(I, mv(1)) + dp(5) * dd%: mc(u) = mc(u) + mc(16): mc(10) = mc(10) + mc(16): GOTO 21
            END IF
            IF dmult(5) = 0 AND dadd(5) > h - 4 AND dadd(5) < 7 THEN
                mv(1) = dadd(5) + 3
                IF b21po% = 1 AND mv(1) = 9 THEN IF mc21po(4) <> 0 THEN f(I, 20) = f(I, 20) + dp(5) * dd%: mc(u) = mc(u) + mc(16): mc(10) = mc(10) + mc(16): GOTO 21
                f(I, mv(1)) = f(I, mv(1)) + dp(5) * dd%: mc(u) = mc(u) + mc(16): mc(10) = mc(10) + mc(16): GOTO 21
            END IF
            IF dd% = 2 THEN
                IF dmult(5) = 0 AND dadd(5) < 7 THEN mv(1) = dadd(5) + 3 ELSE mv(1) = dadd(5) - 7
                IF mv(1) < 0 THEN mv(1) = 0
                f(I, mv(1)) = f(I, mv(1)) + dp(5) * dd%
                mc(u) = mc(u) + mc(16)
                mc(10) = mc(10) + mc(16)
                GOTO 21
            END IF
            IF dd(5) = 1 AND dadd(5) + 3 - k < 10 THEN
                IF split% <> 1 THEN ddn = k / 2 * (19 - k) + dadd(5) + 3 - k: IF br(ddn, m) = 32 THEN dd% = 2 ELSE dd% = 1
                IF split% = 1 AND das% = 1 THEN
                    IF pc1 <= pc2 THEN ddn = pc1 / 2 * (19 - pc1) + dadd(5) + 3 - pc1: IF sar(ddn, m) = 32 THEN dd% = 2 ELSE dd% = 1
                    IF pc2 < pc1 THEN ddn = pc2 / 2 * (19 - pc2) + dadd(5) + 3 - pc2: IF far(ddn, m) = 32 THEN dd% = 2 ELSE dd% = 1
                END IF
                IF split% = 1 AND das% <> 1 THEN dd% = 1
            END IF
            FOR v = 0 TO 9 STEP 1
                dmult(6) = dmult(5) * v: dadd(6) = dadd(5) + v
                IF mc(v) < 1 THEN mc(17) = mc(v) ELSE mc(17) = 1
                IF mc(v) = 0 THEN GOTO 22 ELSE dp(6) = dp(5) * mc(v) / mc(10): mc(v) = mc(v) - mc(17): mc(10) = mc(10) - mc(17)
                IF schw% = 1 THEN IF dadd(6) < 16 THEN f(I, 24) = f(I, 24) + dp(6): mc(v) = mc(v) + mc(17): mc(10) = mc(10) + mc(17): GOTO 22
                IF dadd(6) > g - 7 THEN
                    mv(1) = dadd(6) - 6
                    IF b21po% = 1 AND mv(1) = 9 THEN IF mc21po(5) <> 0 THEN f(I, 21) = f(I, 21) + dp(6) * dd%: mc(v) = mc(v) + mc(17): mc(10) = mc(10) + mc(17): GOTO 22
                    f(I, mv(1)) = f(I, mv(1)) + dp(6) * dd%: mc(v) = mc(v) + mc(17): mc(10) = mc(10) + mc(17): GOTO 22
                END IF
                IF dmult(6) = 0 AND dadd(6) > h - 5 AND dadd(6) < 6 THEN
                    mv(1) = dadd(6) + 4
                    IF b21po% = 1 AND mv(1) = 9 THEN IF mc21po(5) <> 0 THEN f(I, 21) = f(I, 21) + dp(6) * dd%: mc(v) = mc(v) + mc(17): mc(10) = mc(10) + mc(17): GOTO 22
                    f(I, mv(1)) = f(I, mv(1)) + dp(6) * dd%: mc(v) = mc(v) + mc(17): mc(10) = mc(10) + mc(17): GOTO 22
                END IF
                IF dd% = 2 THEN
                    IF dmult(6) = 0 AND dadd(6) < 6 THEN mv(1) = dadd(6) + 4 ELSE mv(1) = dadd(6) - 6
                    IF mv(1) < 0 THEN mv(1) = 0
                    f(I, mv(1)) = f(I, mv(1)) + dp(6) * dd%
                    mc(v) = mc(v) + mc(17)
                    mc(10) = mc(10) + mc(17)
                    GOTO 22
                END IF
                IF dd(6) = 1 AND dadd(6) + 4 - k < 10 THEN
                    IF split% <> 1 THEN ddn = k / 2 * (19 - k) + dadd(6) + 4 - k: IF br(ddn, m) = 32 THEN dd% = 2 ELSE dd% = 1
                    IF split% = 1 AND das% = 1 THEN
                        IF pc1 <= pc2 THEN ddn = pc1 / 2 * (19 - pc1) + dadd(6) + 4 - pc1: IF sar(ddn, m) = 32 THEN dd% = 2 ELSE dd% = 1
                        IF pc2 < pc1 THEN ddn = pc2 / 2 * (19 - pc2) + dadd(6) + 4 - pc2: IF far(ddn, m) = 32 THEN dd% = 2 ELSE dd% = 1
                    END IF
                    IF split% = 1 AND das% <> 1 THEN dd% = 1
                END IF
                FOR w = 0 TO 9 STEP 1
                    dmult(7) = dmult(6) * w: dadd(7) = dadd(6) + w
                    IF mc(w) < 1 THEN mc(12) = mc(w) ELSE mc(12) = 1
                    IF mc(w) = 0 THEN GOTO 23 ELSE dp(7) = dp(6) * mc(w) / mc(10): mc(w) = mc(w) - mc(12): mc(10) = mc(10) - mc(12)
                    IF dadd(7) > g - 8 THEN
                        mv(1) = dadd(7) - 5
                        IF b21po% = 1 AND mv(1) = 9 THEN IF mc21po(6) <> 0 THEN f(I, 22) = f(I, 22) + dp(7) * dd%: mc(w) = mc(w) + mc(12): mc(10) = mc(10) + mc(12): GOTO 23
                        f(I, mv(1)) = f(I, mv(1)) + dp(7) * dd%: mc(w) = mc(w) + mc(12): mc(10) = mc(10) + mc(12): GOTO 23
                    END IF
                    IF dmult(7) = 0 AND dadd(7) > h - 6 AND dadd(7) < 5 THEN
                        mv(1) = dadd(7) + 5
                        IF b21po% = 1 AND mv(1) = 9 THEN IF mc21po(6) <> 0 THEN f(I, 22) = f(I, 22) + dp(7) * dd%: mc(w) = mc(w) + mc(12): mc(10) = mc(10) + mc(12): GOTO 23
                        f(I, mv(1)) = f(I, mv(1)) + dp(7) * dd%: mc(w) = mc(w) + mc(12): mc(10) = mc(10) + mc(12): GOTO 23
                    END IF
                    IF dd% = 2 THEN
                        IF dmult(7) = 0 AND dadd(7) < 5 THEN mv(1) = dadd(7) + 5 ELSE mv(1) = dadd(7) - 5
                        IF mv(1) < 0 THEN mv(1) = 0
                        f(I, mv(1)) = f(I, mv(1)) + dp(7) * dd%
                        mc(w) = mc(w) + mc(12)
                        mc(10) = mc(10) + mc(12)
                        GOTO 23
                    END IF
                    IF dd(7) = 1 AND dadd(7) + 5 - k < 10 THEN
                        IF split% <> 1 THEN ddn = k / 2 * (19 - k) + dadd(7) + 5 - k: IF br(ddn, m) = 32 THEN dd% = 2 ELSE dd% = 1
                        IF split% = 1 AND das% = 1 THEN
                            IF pc1 <= pc2 THEN ddn = pc1 / 2 * (19 - pc1) + dadd(7) + 5 - pc1: IF sar(ddn, m) = 32 THEN dd% = 2 ELSE dd% = 1
                            IF pc2 < pc1 THEN ddn = pc2 / 2 * (19 - pc2) + dadd(7) + 5 - pc2: IF far(ddn, m) = 32 THEN dd% = 2 ELSE dd% = 1
                        END IF
                        IF split% = 1 AND das% <> 1 THEN dd% = 1
                    END IF
                    FOR o = 0 TO 9 STEP 1
                        dmult(8) = dmult(7) * o: dadd(8) = dadd(7) + o
                        IF mc(o) < 1 THEN mc(18) = mc(o) ELSE mc(18) = 1
                        IF mc(o) = 0 THEN GOTO 24 ELSE dp(8) = dp(7) * mc(o) / mc(10): mc(o) = mc(o) - mc(18): mc(10) = mc(10) - mc(18)
                        IF dadd(8) > g - 9 THEN
                            mv(1) = dadd(8) - 4
                            IF b21po% = 1 AND mv(1) = 9 THEN IF mc21po(7) <> 0 THEN f(I, 23) = f(I, 23) + dp(8) * dd%: mc(o) = mc(o) + mc(18): mc(10) = mc(10) + mc(18): GOTO 24
                            f(I, mv(1)) = f(I, mv(1)) + dp(8) * dd%: mc(o) = mc(o) + mc(18): mc(10) = mc(10) + mc(18): GOTO 24
                        END IF
                        IF dmult(8) = 0 AND dadd(8) > h - 7 AND dadd(8) < 4 THEN
                            mv(1) = dadd(8) + 6
                            IF b21po% = 1 AND mv(1) = 9 THEN IF mc21po(7) <> 0 THEN f(I, 23) = f(I, 23) + dp(8) * dd%: mc(o) = mc(o) + mc(18): mc(10) = mc(10) + mc(18): GOTO 24
                            f(I, mv(1)) = f(I, mv(1)) + dp(8) * dd%: mc(o) = mc(o) + mc(18): mc(10) = mc(10) + mc(18): GOTO 24
                        END IF
                        IF dd% = 2 THEN
                            IF dmult(8) = 0 AND dadd(8) < 4 THEN mv(1) = dadd(8) + 6 ELSE mv(1) = dadd(8) - 4
                            IF mv(1) < 0 THEN mv(1) = 0
                            f(I, mv(1)) = f(I, mv(1)) + dp(8) * dd%
                            mc(o) = mc(o) + mc(18)
                            mc(10) = mc(10) + mc(18)
                            GOTO 24
                        END IF
                        mc(o) = mc(o) + mc(18): mc(10) = mc(10) + mc(18): dd% = 1
                    24 NEXT o
                    mc(w) = mc(w) + mc(12): mc(10) = mc(10) + mc(12): dd% = 1
                23 NEXT w
                mc(v) = mc(v) + mc(17): mc(10) = mc(10) + mc(17): dd% = 1
            22 NEXT v
            mc(u) = mc(u) + mc(16): mc(10) = mc(10) + mc(16): dd% = 1
        21 NEXT u
        mc(t) = mc(t) + mc(15): mc(10) = mc(10) + mc(15): dd% = 1
    20 NEXT t
    mc(s) = mc(s) + mc(14): mc(10) = mc(10) + mc(14): dd% = 1
19 NEXT s
18 RETURN
427 v$ = files$ + "fp.txt"
OPEN v$ FOR APPEND AS #1
FOR w = 0 TO 3
    PRINT #1, "             player cards are "; pc1; " and "; pc2; " dealer cards are "; dc1; " and "; dc2
    PRINT #1, "           here is a table displaying a list of the possible player finishes"
    PRINT #1, "           and the percentage chance each of said player finishes will occur"
    PRINT #1, "         when playing decisions are made using hitting strategies ranging from "
    PRINT #1, "                      from stand at "; MID$(o$, w * 35 + 6, 5); " to stand at "; MID$(o$, w * 35 + 36, 5)
    PRINT #1, "                      from stand at "; MID$(o$, w * 35 + 6, 5); " to stand at "; MID$(o$, w * 35 + 41, 5)
    PRINT #1, " "
    PRINT #1, "       ";
    IF w < 3 THEN
        FOR x = 1 TO 7
            PRINT #1, MID$(o$, w * 35 + x * 5 + 1, 5); "   ";
        NEXT x: PRINT #1, " "
        FOR z = 0 TO 16 STEP 1
            PRINT #1, USING "####"; z + 12;
            FOR y = 1 TO 7
                PRINT #1, USING "####.###"; f(w * 7 + y, z) * 100;
        NEXT y: PRINT #1, " ": NEXT z
        IF schw% = 1 THEN PRINT #1, "schw";
        FOR y = 1 TO 7
            IF schw% = 1 THEN PRINT #1, USING "####.###"; f(w * 7 + y, 17) * 100;
        NEXT y: PRINT #1, " "
    END IF
    IF w = 3 THEN
        FOR x = 1 TO 8
            PRINT #1, MID$(o$, w * 35 + x * 5 + 1, 5); "   ";
        NEXT x: PRINT #1, " "
        FOR z = 0 TO 16 STEP 1
            PRINT #1, USING "####"; z + 12;
            FOR y = 1 TO 8
                PRINT #1, USING "####.###"; f(w * 7 + y, z) * 100;
        NEXT y: PRINT #1, " ": NEXT z
        IF schw% = 1 THEN PRINT #1, "schw";
        FOR y = 1 TO 8
            IF schw% = 1 THEN PRINT #1, USING "####.###"; f(w * 7 + y, 17) * 100;
        NEXT y: PRINT #1, " "
    END IF
NEXT w: PRINT #1, " ": PRINT #1, " ": PRINT #1, " ": PRINT #1, " "
CLOSE #1
RETURN
666 dmult(1) = dc1: dadd(1) = dc1
FOR r = 0 TO 9 STEP 1
    dmult(2) = dmult(1) * r: dadd(2) = dadd(1) + r
    IF mc(r) < 1 THEN mc(13) = mc(r) ELSE mc(13) = 1
    IF mc(r) = 0 THEN GOTO 28 ELSE dp(2) = mc(r) / mc(10): mc(r) = mc(r) - mc(13): mc(10) = mc(10) - mc(13)
    IF dadd(2) > 14 THEN mv(1) = dadd(2) - 15: df(mv(1)) = df(mv(1)) + dp(2): mc(r) = mc(r) + mc(13): mc(10) = mc(10) + mc(13): GOTO 28
    IF dmult(2) = 0 AND dadd(2) > j - 1 THEN
        IF dadd(2) <> 9 THEN mv(1) = dadd(2) - 5: df(mv(1)) = df(mv(1)) + dp(2): mc(r) = mc(r) + mc(13): mc(10) = mc(10) + mc(13): GOTO 28
        IF dadd(2) = 9 THEN df(10) = df(10) + dp(2): mc(r) = mc(r) + mc(13): mc(10) = mc(10) + mc(13): GOTO 28
    END IF
    FOR s = 0 TO 9 STEP 1
        dmult(3) = dmult(2) * s: dadd(3) = dadd(2) + s
        IF mc(s) < 1 THEN mc(14) = mc(s) ELSE mc(14) = 1
        IF mc(s) = 0 THEN GOTO 29 ELSE dp(3) = dp(2) * mc(s) / mc(10): mc(s) = mc(s) - mc(14): mc(10) = mc(10) - mc(14)
        IF dadd(3) > 13 THEN mv(1) = dadd(3) - 14: df(mv(1)) = df(mv(1)) + dp(3): mc(s) = mc(s) + mc(14): mc(10) = mc(10) + mc(14): GOTO 29
        IF dmult(3) = 0 AND dadd(3) > j - 2 AND dadd(3) < 9 THEN mv(1) = dadd(3) - 4: df(mv(1)) = df(mv(1)) + dp(3): mc(s) = mc(s) + mc(14): mc(10) = mc(10) + mc(14): GOTO 29
        FOR t = 0 TO 9 STEP 1
            dmult(4) = dmult(3) * t: dadd(4) = dadd(3) + t
            IF mc(t) < 1 THEN mc(15) = mc(t) ELSE mc(15) = 1
            IF mc(t) = 0 THEN GOTO 30 ELSE dp(4) = dp(3) * mc(t) / mc(10): mc(t) = mc(t) - mc(15): mc(10) = mc(10) - mc(15)
            IF dadd(4) > 12 THEN mv(1) = dadd(4) - 13: df(mv(1)) = df(mv(1)) + dp(4): mc(t) = mc(t) + mc(15): mc(10) = mc(10) + mc(15): GOTO 30
            IF dmult(4) = 0 AND dadd(4) > j - 3 AND dadd(4) < 8 THEN mv(1) = dadd(4) - 3: df(mv(1)) = df(mv(1)) + dp(4): mc(t) = mc(t) + mc(15): mc(10) = mc(10) + mc(15): GOTO 30
            FOR u = 0 TO 9 STEP 1
                dmult(5) = dmult(4) * u: dadd(5) = dadd(4) + u
                IF mc(u) < 1 THEN mc(16) = mc(u) ELSE mc(16) = 1
                IF mc(u) = 0 THEN GOTO 31 ELSE dp(5) = dp(4) * mc(u) / mc(10): mc(u) = mc(u) - mc(16): mc(10) = mc(10) - mc(16)
                IF dadd(5) > 11 THEN mv(1) = dadd(5) - 12: df(mv(1)) = df(mv(1)) + dp(5): mc(u) = mc(u) + mc(16): mc(10) = mc(10) + mc(16): GOTO 31
                IF dmult(5) = 0 AND dadd(5) > j - 4 AND dadd(5) < 7 THEN mv(1) = dadd(5) - 2: df(mv(1)) = df(mv(1)) + dp(5): mc(u) = mc(u) + mc(16): mc(10) = mc(10) + mc(16): GOTO 31
                FOR v = 0 TO 9 STEP 1
                    dmult(6) = dmult(5) * v: dadd(6) = dadd(5) + v
                    IF mc(v) < 1 THEN mc(17) = mc(v) ELSE mc(17) = 1
                    IF mc(v) = 0 THEN GOTO 32 ELSE dp(6) = dp(5) * mc(v) / mc(10): mc(v) = mc(v) - mc(17): mc(10) = mc(10) - mc(17)
                    IF dadd(6) > 10 THEN mv(1) = dadd(6) - 11: df(mv(1)) = df(mv(1)) + dp(6): mc(v) = mc(v) + mc(17): mc(10) = mc(10) + mc(17): GOTO 32
                    IF dmult(6) = 0 AND dadd(6) > j - 5 AND dadd(6) < 6 THEN mv(1) = dadd(6) - 1: df(mv(1)) = df(mv(1)) + dp(6): mc(v) = mc(v) + mc(17): mc(10) = mc(10) + mc(17): GOTO 32
                    FOR w = 0 TO 9 STEP 1
                        dmult(7) = dmult(6) * w: dadd(7) = dadd(6) + w
                        IF mc(w) < 1 THEN mc(12) = mc(w) ELSE mc(12) = 1
                        IF mc(w) = 0 THEN GOTO 33 ELSE dp(7) = dp(6) * mc(w) / mc(10): mc(w) = mc(w) - mc(12): mc(10) = mc(10) - mc(12)
                        IF dadd(7) > 9 THEN mv(1) = dadd(7) - 10: df(mv(1)) = df(mv(1)) + dp(7): mc(w) = mc(w) + mc(12): mc(10) = mc(10) + mc(12): GOTO 33
                        IF dmult(7) = 0 AND dadd(7) > j - 6 AND dadd(7) < 5 THEN mv(1) = dadd(7): df(mv(1)) = df(mv(1)) + dp(7): mc(w) = mc(w) + mc(12): mc(10) = mc(10) + mc(12): GOTO 33
                        FOR o = 0 TO 9 STEP 1
                            dmult(8) = dmult(7) * o: dadd(8) = dadd(7) + o
                            IF mc(o) < 1 THEN mc(18) = mc(o) ELSE mc(18) = 1
                            IF mc(o) = 0 THEN GOTO 34 ELSE dp(8) = dp(7) * mc(o) / mc(10): mc(o) = mc(o) - mc(18): mc(10) = mc(10) - mc(18)
                            IF dadd(8) > 8 THEN mv(1) = dadd(8) - 9: df(mv(1)) = df(mv(1)) + dp(8): mc(o) = mc(o) + mc(18): mc(10) = mc(10) + mc(18): GOTO 34
                            IF dmult(8) = 0 AND dadd(8) > j - 7 AND dadd(8) < 4 THEN mv(1) = dadd(8) + 1: df(mv(1)) = df(mv(1)) + dp(8): mc(o) = mc(o) + mc(18): mc(10) = mc(10) + mc(18): GOTO 34
                            mc(o) = mc(o) + mc(18): mc(10) = mc(10) + mc(18)
                        34 NEXT o
                        mc(w) = mc(w) + mc(12): mc(10) = mc(10) + mc(12)
                    33 NEXT w
                    mc(v) = mc(v) + mc(17): mc(10) = mc(10) + mc(17)
                32 NEXT v
                mc(u) = mc(u) + mc(16): mc(10) = mc(10) + mc(16)
            31 NEXT u
            mc(t) = mc(t) + mc(15): mc(10) = mc(10) + mc(15)
        30 NEXT t
        mc(s) = mc(s) + mc(14): mc(10) = mc(10) + mc(14)
    29 NEXT s
    mc(r) = mc(r) + mc(13): mc(10) = mc(10) + mc(13)
28 NEXT r
FOR y = 0 TO 10
df(11) = df(11) + df(y): NEXT y
FOR y = 1 TO 5
df(12) = df(12) + df(4 + y): NEXT y
RETURN
888 IF pc1 <= pc2 THEN n = pc1 / 2 * (19 - pc1) + pc2 ELSE n = pc2 / 2 * (19 - pc2) + pc1
x = 0
IF split% <> 1 THEN hit(n, dc1) = -3 ELSE splithands(pc2, dc1) = -3
FOR v = 12 TO 19
    FOR w = 5 TO 8
        IF w + 12 < v THEN w = v - 12
        x = x + 1
        IF v >= hl% AND v <= hu% AND w + 12 >= sl% AND w + 12 <= su% THEN
            q = 0
            IF bjsh% <> 1 THEN f(x, 9) = f(x, 9) + f(x, 17): f(x, 17) = 0
            IF po777 = 0 THEN f(x, 9) = f(x, 9) + f(x, 18): f(x, 18) = 0
            IF po678 = 0 THEN f(x, 9) = f(x, 9) + f(x, 19): f(x, 19) = 0
            IF mc21po(4) = 0 THEN f(x, 9) = f(x, 9) + f(x, 20): f(x, 20) = 0
            IF mc21po(5) = 0 THEN f(x, 9) = f(x, 9) + f(x, 21): f(x, 21) = 0
            IF mc21po(6) = 0 THEN f(x, 9) = f(x, 9) + f(x, 22): f(x, 22) = 0
            IF mc21po(7) = 0 THEN f(x, 9) = f(x, 9) + f(x, 23): f(x, 23) = 0
            IF dc1 = 0 AND push% = 1 THEN df(4) = df(4) + df(10): df(10) = 0
            IF dc1 = 9 AND ten% = 1 THEN df(4) = df(4) + df(10): df(10) = 0
            FOR y = 0 TO 16
                IF y > 9 THEN
                    FOR z = 0 TO 4
                        q = q - f(x, y) * df(z)
                    NEXT z
                    FOR z = 1 TO 5
                        IF dbc% <> 1 AND dbc% <> 2 THEN q = q - f(x, y) * df(4 + z)
                        IF dbc% = 1 OR dbc% = 2 THEN
                            IF z + 21 < y + 12 THEN q = q - f(x, y) * df(4 + z)
                            IF z + 21 = y + 12 THEN
                                IF dwt% = 1 THEN q = q - f(x, y) * df(4 + z)
                                IF dwt% <> 1 AND dbc% = 1 THEN q = q - f(x, y) * df(4 + z) * .5
                            END IF
                            IF z + 21 > y + 12 AND dbc% = 2 THEN q = q + f(x, y) * df(4 + z)
                        END IF
                    NEXT z
                END IF
                IF y < 10 THEN
                    FOR z = 0 TO 5
                        IF z = 5 THEN q = q + f(x, y) * df(12)
                        IF z < 5 THEN IF z < y - 5 THEN q = q + f(x, y) * df(z)
                        IF z < 5 THEN IF z > y - 5 THEN q = q - f(x, y) * df(z)
                        IF z < 4 THEN IF z = y - 5 THEN IF dwt% = 1 THEN q = q - f(x, y) * df(z)
                        IF z = 4 AND y = 9 THEN
                            IF immed% = 1 THEN q = q + f(x, y) * df(4)
                            IF immed% <> 1 AND dwt% = 1 THEN q = q - f(x, y) * df(4)
                        END IF
                    NEXT z
                END IF
            NEXT y
            q = q + f(x, 17) * blackjack + f(x, 18) * (po777 + 1) + f(x, 19) * (po678 + 1) + f(x, 20) * (mc21po(4) + 1) + f(x, 21) * (mc21po(5) + 1) + f(x, 22) * (mc21po(6) + 1) + f(x, 23) * (mc21po(7) + 1) + f(x, 24)
            IF split% <> 1 THEN
                q = q - df(10)
                aq(n, dc1, x) = q
                IF q > hit(n, dc1) THEN hit(n, dc1) = q: br(n, dc1) = x
            END IF
            IF split% = 1 THEN
                IF pc2 < pc1 THEN IF q > splithands(pc2, dc1) THEN splithands(pc2, dc1) = q: far(n, dc1) = x
                IF pc2 = pc1 THEN IF q > splithands(pc2, dc1) THEN splithands(pc2, dc1) = q: far(n, dc1) = x: sar(n, dc1) = x
                IF pc1 < pc2 THEN IF q > splithands(pc2, dc1) THEN splithands(pc2, dc1) = q: sar(n, dc1) = x
            END IF
            IF pc1 * pc2 = 0 THEN
                IF pc1 + pc2 > 5 AND w < 6 THEN IF x < 25 THEN x = x + 1: w = w + 1: IF split% <> 1 THEN aq(n, dc1, x) = q
                IF pc1 + pc2 > 6 AND w < 7 THEN IF x < 28 THEN x = x + 1: w = w + 1: IF split% <> 1 THEN aq(n, dc1, x) = q
                IF pc1 + pc2 > 7 AND w < 8 THEN x = x + 1: w = w + 1: IF split% <> 1 THEN aq(n, dc1, x) = q
            END IF
            IF pc1 * pc2 <> 0 THEN
                IF pc1 + pc2 > 4 AND w < 6 THEN IF x < 25 THEN x = x + 1: w = w + 1: IF split% <> 1 THEN aq(n, dc1, x) = q
                IF pc1 + pc2 > 5 AND w < 7 THEN IF x < 28 THEN x = x + 1: w = w + 1: IF split% <> 1 THEN aq(n, dc1, x) = q
                IF pc1 + pc2 > 6 AND w < 8 THEN x = x + 1: w = w + 1: IF split% <> 1 THEN aq(n, dc1, x) = q
            END IF
        END IF
NEXT w: NEXT v: account = 0: q = 0
IF split% <> 1 THEN standr% = br(n, dc1): stand = hit(n, dc1): REM PRINT MID$(o$, br(n, dc1) * 5 + 1, 5); " stand strategy yields a win minus loss expectation of"; : PRINT USING "##.####"; hit(n, dc1)
RETURN
222 FOR x = 0 TO 9
    dadd(0) = pc1 + pc2 + x: dmult(0) = pc1 * pc2 * x: IF dadd(0) > 26 THEN dadd(0) = 26
    IF dmult(0) <> 0 THEN IF dadd(0) > 8 THEN f(0, dadd(0) - 9) = f(0, dadd(0) - 9) + mc(x) / mc(10) ELSE f(0, 0) = f(0, 0) + mc(x) / mc(10)
    IF dmult(0) = 0 THEN IF dadd(0) > 3 THEN IF dadd(0) < 9 THEN f(0, dadd(0) + 1) = f(0, dadd(0) + 1) + mc(x) / mc(10) ELSE f(0, dadd(0) - 9) = f(0, dadd(0) - 9) + mc(x) / mc(10)
    IF dmult(0) = 0 THEN IF dadd(0) < 4 THEN f(0, 0) = f(0, 0) + mc(x) / mc(10)
NEXT x
FOR y = 0 TO 17
    IF split% <> 1 THEN pbr(n, dc1, 32, y) = f(0, y)
    IF split% = 1 THEN
        IF pc1 > pc2 THEN pfar(n, dc1, 32, y) = f(0, y)
        IF pc1 = pc2 THEN pfar(n, dc1, 32, y) = f(0, y): psar(n, dc1, 32, y) = f(0, y)
        IF pc2 > pc1 THEN psar(n, dc1, 32, y) = f(0, y)
    END IF
    IF y > 9 THEN
        FOR z = 0 TO 4
            q = q - f(0, y) * df(z)
        NEXT z
        FOR z = 1 TO 5
            IF dbc% <> 1 AND dbc% <> 2 THEN q = q - f(0, y) * df(4 + z)
            IF dbc% = 1 OR dbc% = 2 THEN
                IF z + 21 < y + 12 THEN q = q - f(0, y) * df(4 + z)
                IF z + 21 = y + 12 THEN
                    IF dwt% = 1 THEN q = q - f(0, y) * df(4 + z)
                    IF dwt% <> 1 AND dbc% = 1 THEN q = q - f(0, y) * df(4 + z) * .5
                END IF
                IF z + 21 > y + 12 AND dbc% = 2 THEN q = q + f(0, y) * df(4 + z)
            END IF
        NEXT z
    END IF
    IF y < 10 THEN
        FOR z = 0 TO 5
            IF z = 5 THEN q = q + f(0, y) * df(12)
            IF z < 5 THEN IF z < y - 5 THEN q = q + f(0, y) * df(z)
            IF z < 5 THEN IF z > y - 5 THEN q = q - f(0, y) * df(z)
            IF z < 4 THEN IF z = y - 5 THEN IF dwt% = 1 THEN q = q - f(0, y) * df(z)
            IF z = 4 AND y = 9 THEN
                IF immed% = 1 THEN q = q + f(0, y) * df(4)
                IF immed% <> 1 AND dwt% = 1 THEN q = q - f(0, y) * df(4)
            END IF
        NEXT z
    END IF
NEXT y
IF split% <> 1 THEN
    IF hole% <> 1 THEN q = 2 * q - df(10)
    IF hole% = 1 THEN q = 2 * (q - df(10))
    aq(n, dc1, 32) = q
    IF q > hit(n, dc1) THEN hit(n, dc1) = q: br(n, dc1) = 32
    ddown = q: REM PRINT "double down strategy yields a win minus loss expectation of"; : PRINT USING "##.####"; q
END IF
IF split% = 1 THEN
    IF hole% <> 1 THEN q = 2 * q
    IF hole% = 1 THEN q = 2 * (q - df(10))
    IF pc1 > pc2 THEN IF q > splithands(pc2, dc1) THEN splithands(pc2, dc1) = q: far(n, dc1) = 32
    IF pc2 = pc1 THEN IF q > splithands(pc2, dc1) THEN splithands(pc2, dc1) = q: far(n, dc1) = 32: sar(n, dc1) = 32
    IF pc2 > pc1 THEN IF q > splithands(pc2, dc1) THEN splithands(pc2, dc1) = q: sar(n, dc1) = 32
END IF
RETURN
234 IF (split% <> 1 AND ls% = 1) THEN
    q = -.5
    IF dc1 = 0 AND early% <> 1 THEN q = q * (1 - mc9 / mc10) - mc9 / mc10
    IF dc1 = 9 AND early% <> 1 THEN q = q * (1 - mc0 / mc10) - mc0 / mc10
    aq(n, dc1, 31) = q
    IF q > hit(n, dc1) THEN hit(n, dc1) = q: br(n, dc1) = 31
    IF early% <> 1 THEN srndr = q: REM PRINT "L surrender strategy yields a win minus loss expectation of"; : PRINT USING "##.####"; q
    IF early% = 1 THEN srndr = q: REM PRINT "E surrender strategy yields a win minus loss expectation of"; : PRINT USING "##.####"; q
END IF
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
pshc(2, 2) = pshc(2, 0) * .5
pshc(3, 0) = x(0) / y(0) * x(1) / y(1)
pshc(3, 1) = 2 * pshc(3, 0)
pshc(3, 2) = pshc(3, 0)
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
pshc(3, 2) = pshc(3, 0) / 3
pshc(4, 0) = 4 * (1 - x(0) / y(0)) * x(0) / y(1) * x(1) / y(2) * x(2) / y(3)
pshc(4, 1) = 3 * pshc(4, 0)
pshc(4, 2) = 2 * pshc(4, 0) / 3
pshc(5, 0) = x(0) / y(0) * x(1) / y(1) * x(2) / y(2) * x(3) / y(3)
pshc(5, 1) = 3 * pshc(5, 0)
pshc(5, 2) = pshc(5, 0)
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
pshc(4, 2) = pshc(4, 0) * .25
pshc(5, 0) = 14 * (1 - x(0) / y(0)) * (1 - x(0) / y(1)) * x(0) / y(2)
pshc(5, 0) = pshc(5, 0) * x(1) / y(3) * x(2) / y(4) * x(3) / y(5)
pshc(5, 1) = 4 * pshc(5, 0)
pshc(5, 2) = pshc(5, 0) * .5
pshc(6, 0) = 6 * (1 - x(0) / y(0)) * x(0) / y(1) * x(1) / y(2) * x(2) / y(3)
pshc(6, 0) = phsc(6, 0) * x(3) / y(4) * x(4) / y(5)
pshc(6, 1) = 4 * pshc(6, 0)
pshc(6, 2) = pshc(6, 0) * .75
pshc(7, 0) = x(0) / y(0) * x(1) / y(1) * x(2) / y(2) * x(3) / y(3) * x(4) / y(4) * x(5) / y(5)
pshc(7, 1) = 4 * pshc(7, 1)
pshc(7, 2) = pshc(7, 1)
FOR z = 1 TO 7
    pshc(0, 0) = pshc(0, 0) + pshc(z, 0)
    pshc(0, 1) = pshc(0, 1) + pshc(z, 1)
    pshc(0, 2) = pshc(0, 2) + pshc(z, 2)
    REM PRINT pshc(0, 0); pshc(0, 1); pshc(0, 2): SLEEP 2
NEXT z
RETURN
226 CLS:
PRINT "ULTIMA282.BAS asks the user to enter the folder/filename prefix of the"
PRINT "files to be used to store application output. ULTIMA282.BAS prompts the"
PRINT "user to define the rules of a card game belonging to the Blackjack family"
PRINT "of table-card games. ULTIMA282.BAS conducts a math analysis of the card game"
PRINT "by means of the abbreviated-combinatorial-analysis method. ULTIMA282.BAS"
PRINT "assembles the results of the math analysis into a set of tables. The set of"
PRINT "tables consists of a set of combination-dependent-playing-strategy tables,"
PRINT "and a combination-dependent-expected-value table. ULTIMA282.BAS calculates"
PRINT "an estimate of the expected value of the game and adds it to the bottom of"
PRINT "the combination-dependent-expected value table. ULTIMA282.BAS stores the"
PRINT "user-defined set of game rules and the set of tables in a set of files."
PRINT "ULTIMA283.BAS can load the data contained in these files into program"
PRINT "memory and use the data to conduct a simulation of the card game wherein;"
PRINT "the computer-controlled player uses the set of strategy tables to make"
PRINT "decisions on how to play the player's hand. ULTIMA293.BAS can load the"
PRINT "data contained in these files into program memory and use the data to"
PRINT "assemble a total-dependent-basic-strategy table."
PRINT " "
o$ = "     12/A612/A712/A812/A913/A613/A713/A813/A914/A614/A714/A814/A915/A615/A715/A815/A916/A616/A716/A816/A917/A617/A717/A817/A918/A718/A818/A919/A819/A9SplitSrndrDDown"
n$ = "     12   12   12   12   13   13   13   13   14   14   14   14   15   15   15   15   16   16   16   16   17   17   17   17   18   18   18   19   19   SplitSrndrDDown"
999 PRINT "enter the folder/filename prefix of the files to be used":
PRINT "to store application output": INPUT files$
PRINT " "
PRINT "press 'y' and enter for a standard deck composition";: INPUT ans$
v$ = files$ + "i.txt"
OPEN v$ FOR OUTPUT AS #1
PRINT #1, "ULTIMA282.BAS asks the user to enter the folder/filename prefix of the"
PRINT #1, "files to be used to store application output. ULTIMA282.BAS prompts the"
PRINT #1, "user to define the rules of a card game belonging to the Blackjack family"
PRINT #1, "of table-card games. ULTIMA282.BAS conducts a math analysis of the card game"
PRINT #1, "by means of the abbreviated-combinatorial-analysis method. ULTIMA282.BAS"
PRINT #1, "assembles the results of the math analysis into a set of tables. The set of"
PRINT #1, "tables consists of a set of combination-dependent-playing-strategy tables,"
PRINT #1, "and a combination-dependent-expected-value table. ULTIMA282.BAS calculates"
PRINT #1, "an estimate of the expected value of the game and adds it to the bottom of"
PRINT #1, "the combination-dependent-expected value table. ULTIMA282.BAS stores the"
PRINT #1, "user-defined set of game rules and the set of tables in a set of files."
PRINT #1, "ULTIMA283.BAS can load the data contained in these files into program"
PRINT #1, "memory and use the data to conduct a simulation of the card game wherein;"
PRINT #1, "the computer-controlled player uses the set of strategy tables to make"
PRINT #1, "decisions on how to play the player's hand. ULTIMA293.BAS can load the"
PRINT #1, "data contained in these files into program memory and use the data to"
PRINT #1, "assemble a total-dependent-basic-strategy table."
PRINT #1, " "
PRINT #1, "enter the folder/filename prefix of the files to be used":
PRINT #1, "to store application output ";: PRINT #1, files$
PRINT #1, " "
CLOSE #1
pdenom(0) = 65: pdenom(1) = 50: pdenom(2) = 51: pdenom(3) = 52: pdenom(4) = 53: pdenom(5) = 54: pdenom(6) = 55
pdenom(7) = 56: pdenom(8) = 57: pdenom(9) = 84: pdenom(10) = 74: pdenom(11) = 81: pdenom(12) = 75
IF ans$ <> "y" THEN
    PRINT "enter deck composition ";
    FOR x = 0 TO 12
    PRINT "input the number of cards of denomination "; CHR$(pdenom(x)); " ";: INPUT a: card(x) = a: card(13) = card(13) + a: NEXT x
END IF
IF ans$ = "y" THEN
    FOR x = 0 TO 12
    card(x) = 4: NEXT x: card(13) = 52
END IF
OPEN v$ FOR APPEND AS #1
PRINT #1, "press 'y' and enter for a standard deck composition ";: PRINT #1, ans$
IF ans$ <> "y" THEN
    PRINT #1, "enter deck composition ";
    FOR x = 0 TO 12
    PRINT #1, "input the number of cards of denomination "; CHR$(pdenom(x)); " ";: PRINT #1, card(x): NEXT x
END IF
CLOSE #1
PRINT "input the number of decks used";: INPUT deck%
PRINT "what is the percentage of the shoe remaining at point of reshuffle.(15-100)";: INPUT shf
PRINT "input dealer soft finish";: INPUT j: j = j - 12
PRINT "press 1 and enter for dealer wins ties";: INPUT dwt%
PRINT "press 1 and enter for a double bust contest where in if the player"
PRINT "finishes closer to twenty-one than the dealer, the player pushes"
PRINT "press 2 and enter for a double bust contest where in if the player"
PRINT "finishes closer to twenty-one than the dealer, the player wins";: INPUT dbc%
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
    PRINT "press 1 and enter to draw to split aces";: INPUT dsa%
END IF
PRINT "press 1 and enter to restrict use of double down to specific hand totals";: INPUT dh%
IF dh% = 1 THEN PRINT "enter 9 for 9/10/11 only, enter 10 for 10/11 only, enter 11 for 11 only ";: INPUT ld%
IF NOT dh% = 1 THEN ld% = 1
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
    IF card(6) <> 0 THEN
        mc21po(1) = (card(6) * deck%) * (card(6) * deck% - 1) * (card(6) * deck% - 2): mc21po(2) = deck% * (deck% - 1) * (deck% - 2) * 4 / mc21po(1)
        mc21po(3) = deck% * (deck% - 1) * (deck% - 2) / mc21po(1): mc21po(1) = ms777
        mc21po(11) = (jp777 - ms777) * mc21po(2): mc21po(3) = (sp777 - ss777) * mc21po(3): mc21po(2) = (ss777 - ms777) * mc21po(2)
    END IF
    IF card(5) * card(6) * card(7) <> 0 THEN
        mc21po(8) = card(5) * card(6) * card(7) * deck% ^ 3: mc21po(9) = deck% ^ 3 * 4 / mc21po(8)
        mc21po(10) = deck% ^ 3 * 1 / mc21po(8): mc21po(8) = ms678
        mc21po(9) = (ss678 - ms678) * mc21po(9): mc21po(10) = (sp678 - ss678) * mc21po(10)
    END IF
END IF
PRINT "press 1 and enter for double on more than two cards";: INPUT dd%
IF dd% = 1 THEN
    FOR x = 3 TO 7
    PRINT "press 1 and enter for double on "; x; " cards";: INPUT dd(x): NEXT x
END IF
PRINT "press 1 and enter to save to a text file tables listing "
PRINT "player-finish probabilities calculated for each of the five-hundred-and-fifty"
PRINT "combinations of point values that could be assigned to a set of cards consisting"
PRINT "of the two cards in the player's inital hand and the one card dealt face up in"
PRINT "the dealer's initial hand";: INPUT pfpt%
IF pfpt% = 1 THEN
    v$ = files$ + "fp.txt"
    OPEN v$ FOR OUTPUT AS #1
    PRINT #1, "Tables listing player-finish probabilities calculated for each of the"
    PRINT #1, "five-hundred-and-fifty combinations of point values that could be assigned"
    PRINT #1, "to a set of cards consisting of the two cards in the player's inital hand"
    PRINT #1, "and the one card dealt face up in the dealer's initial hand"
    PRINT #1, q$
    PRINT #1, ""
    CLOSE #1
END IF
PRINT "type and enter a brief description of rules": INPUT q$
hl% = 12: hu% = 19: sl% = 17: su% = 20
v$ = files$ + "i.txt"
OPEN v$ FOR APPEND AS #1
PRINT #1, "input the number of decks used ";: PRINT #1, deck%
PRINT #1, "what is the percentage of the shoe remaining at point of reshuffle.(15-100) ";: PRINT #1, shf
PRINT #1, "input dealer soft finish ";: PRINT #1, j + 12
PRINT #1, "press 1 and enter for dealer wins ties ";: PRINT #1, dwt%
PRINT #1, "press 1 and enter for a double bust contest where in if the player"
PRINT #1, "finishes closer to twenty-one than the dealer, the player pushes"
PRINT #1, "press 2 and enter for a double bust contest where in if the player"
PRINT #1, "finishes closer to twenty-one than the dealer, the player wins ";: PRINT #1, dbc%
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
    PRINT #1, "press 1 and enter to draw to split aces ";: PRINT #1, dsa%
END IF
PRINT #1, "press 1 and enter to restrict use of double down to specific hand totals ";: PRINT #1, dh%
IF dh% = 1 THEN PRINT #1, "enter 9 for 9/10/11 only, enter 10 for 10/11 only, enter 11 for 11 only ";: PRINT #1, ld%
IF NOT dh% = 1 THEN ld% = 1
PRINT #1, "input 2 for blackjack pays 2:1 or 1.5 for pays 3:2 or 1 for pays 1:1 ";: PRINT #1, blackjack
PRINT #1, "press 1 and enter for blackjack pays off on split hands ";: PRINT #1, bjsh%
PRINT #1, "press 1 and enter for all player hands of 21 pay off immediately ";: PRINT #1, immed%
PRINT #1, "press 1 and enter for player blackjack beats dealer blackjack ";: PRINT #1, pbd%
PRINT #1, "press 1 and enter for bonus 21 payoffs ";: PRINT #1, b21po%
IF b21po% = 1 THEN
    FOR x = 5 TO 8
        PRINT #1, "enter payoff for"; x; " card 21 ";: PRINT #1, mc21po(x - 1)
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
PRINT #1, "press 1 and enter for double on more than two cards ";: PRINT #1, dd%
IF dd% = 1 THEN
    FOR x = 3 TO 7
    PRINT #1, "press 1 and enter for double on "; x; " cards ";: PRINT #1, dd(x): NEXT x
END IF
PRINT #1, ""
PRINT #1, "press 1 and enter to save to a text file tables listing "
PRINT #1, "player finish probabilities calculated during the operation"
PRINT #1, "of the program in the process of finding the best of twenty nine"
PRINT #1, "hitting strategies (ranging from stand at 12/A6 to stand at 19/A9)"
PRINT #1, "for each of five hundred and fifty unique combinations"
PRINT #1, "of initial two card player hand and dealer face up card. ";: PRINT #1, pfpt%
PRINT #1, "type and enter a brief description of rules ": PRINT #1, q$
PRINT #1, ""
PRINT #1, ""
PRINT #1, "START: "; DATE$; " "; TIME$
dd% = 1
CLOSE #1
v$ = files$ + "ev.txt"
OPEN v$ FOR OUTPUT AS #1
PRINT #1, "the most favorable player responses ": PRINT #1, q$
PRINT #1, ""
CLOSE #1
z = 0
FOR x = 12 TO 19
    FOR y = 5 TO 8
        IF x > y + 12 THEN y = x - 12
        z = z + 1
        g(z) = x: h(z) = y
NEXT y: NEXT x: g(30) = 1000: g(31) = 1000: h(30) = 1000: h(31) = 1000: h(32) = 1000: g(32) = 1000
FOR k = 9 TO 0 STEP -1
    FOR l = 9 TO k STEP -1
        n = k / 2 * (19 - k) + l
        FOR m = 9 TO 0 STEP -1
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
            IF (pc1 = 0 AND pc2 = 9) OR (pc1 = 9 AND pc2 = 0) THEN
                q = blackjack
                IF pbd% <> 1 THEN
                    IF dc1 = 0 THEN q = q * (1 - mc(9) / mc(10)): IF dwt% = 1 THEN q = q - mc(9) / mc(10)
                    IF dc1 = 9 THEN q = q * (1 - mc(0) / mc(10)): IF dwt% = 1 THEN q = q - mc(0) / mc(10)
                END IF
                CLS: REM PRINT "blackjack yields a win minus loss expectation of"; : PRINT USING "##.####"; q
                hit(9, dc1) = q: br(9, dc1) = 1: far(9, dc1) = 1: sar(9, dc1) = 1: n = 9: aq(n, dc1, 1) = q: GOTO 333
            END IF
            GOSUB 666
            FOR y = 0 TO 12
            dbr(n, dc1, y) = df(y): NEXT y
            ERASE mc
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
            IF hole% <> 1 AND push% <> 1 AND dc1 = 0 THEN
                FOR x = 0 TO 8
                    mc(x) = mc(x) * (mc(10) - mc(9) - 1) / (mc(10) - mc(9))
                NEXT x: mc(10) = mc(10) - 1
            END IF
            IF hole% <> 1 AND ten% <> 1 AND dc1 = 9 THEN
                FOR x = 1 TO 9
                    mc(x) = mc(x) * (mc(10) - mc(0) - 1) / (mc(10) - mc(0))
                NEXT x: mc(10) = mc(10) - 1
            END IF
            FOR g = 12 TO 19
                FOR h = 5 TO 8
                    IF h + 12 < g THEN h = g - 12
                    I = I + 1
                    IF g >= hl% AND g <= hu% AND h + 12 >= sl% AND h + 12 <= su% THEN
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
            FOR x = 0 TO 29
                FOR y = 0 TO 24
                    pbr(n, dc1, x, y) = f(x, y)
            NEXT y: NEXT x
            IF pfpt% = 1 THEN GOSUB 427
            GOSUB 888
            IF dh% <> 1 THEN GOSUB 222
            IF dh% = 1 THEN IF (12 > pc1 + pc2 + 2 AND pc1 + pc2 + 2 > ld% - 1) THEN GOSUB 222
            IF ls% = 1 THEN GOSUB 234
            ERASE df: ERASE f
            IF (pc1 <> pc2 OR splt% <> 1) THEN GOTO 333 ELSE split% = 1
            pc3 = pc2
            FOR pc2 = 9 TO 0 STEP -1
                IF pc1 <= pc2 THEN n = pc1 / 2 * (19 - pc1) + pc2 ELSE n = pc2 / 2 * (19 - pc2) + pc1
                IF pc1 <> 0 OR dsa% = 1 THEN
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
                    IF mc(pc2) < 1 THEN mc(19) = mc(pc2) ELSE mc(19) = 1
                    IF mc(pc2) = 0 THEN GOTO 444 ELSE mc(pc2) = mc(pc2) - mc(19): mc(10) = mc(10) - mc(19)
                    GOSUB 666
                END IF
                IF pc1 > pc2 THEN
                    FOR y = 0 TO 12
                    dfar(n, dc1, y) = df(y): NEXT y
                END IF
                IF pc1 = pc2 THEN
                    FOR y = 0 TO 12
                    dfar(n, dc1, y) = df(y): dsar(n, dc1, y) = df(y): NEXT y
                END IF
                IF pc2 > pc1 THEN
                    FOR y = 0 TO 12
                    dsar(n, dc1, y) = df(y): NEXT y
                END IF
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
                IF hole% <> 1 AND push% <> 1 AND dc1 = 0 THEN
                    FOR x = 0 TO 8
                        mc(x) = mc(x) * (mc(10) - mc(9) - 1) / (mc(10) - mc(9))
                    NEXT x: mc(10) = mc(10) - 1
                END IF
                IF hole% <> 1 AND ten% <> 1 AND dc1 = 9 THEN
                    FOR x = 1 TO 9
                        mc(x) = mc(x) * (mc(10) - mc(0) - 1) / (mc(10) - mc(0))
                    NEXT x: mc(10) = mc(10) - 1
                END IF
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
                IF pc2 <> pc1 THEN prob = mc(pc2) / mc(10) / (1 - mc(pc1) / mc(10) + pshc(0, 2))
                IF pc2 = pc1 THEN prob = pshc(0, 2) / (1 - mc(pc1) / mc(10) + pshc(0, 2))
                IF pc1 = 0 AND dsa% <> 1 THEN dadd(0) = pc1 + pc2: f(0, dadd(0)) = f(0, dadd(0)) + prob
                probs = probs + prob
                IF mc(pc2) < 1 THEN mc(19) = mc(pc2) ELSE mc(19) = 1
                IF mc(pc2) = 0 THEN GOTO 444
                mc(pc2) = mc(pc2) - mc(19): mc(10) = mc(10) - mc(19)
                IF pc1 <> 0 OR dsa% = 1 THEN
                    FOR g = 12 TO 19
                        FOR h = 5 TO 8
                            IF h + 12 < g THEN h = g - 12
                            I = I + 1
                            IF g >= hl% AND g <= hu% AND h + 12 >= sl% AND h + 12 <= su% THEN
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
                    IF pc1 > pc2 THEN
                        FOR x = 0 TO 29
                            FOR y = 0 TO 24
                                pfar(n, dc1, x, y) = f(x, y)
                        NEXT y: NEXT x
                    END IF
                    IF pc1 = pc2 THEN
                        FOR x = 0 TO 29
                            FOR y = 0 TO 24
                                pfar(n, dc1, x, y) = f(x, y): psar(n, dc1, x, y) = f(x, y)
                        NEXT y: NEXT x
                    END IF
                    IF pc2 > pc1 THEN
                        FOR x = 0 TO 29
                            FOR y = 0 TO 24
                                psar(n, dc1, x, y) = f(x, y)
                        NEXT y: NEXT x
                    END IF
                    GOSUB 888
                    IF das% = 1 THEN
                        IF dh% <> 1 THEN GOSUB 222
                        IF dh% = 1 THEN IF (12 > pc1 + pc2 + 2 AND pc1 + pc2 + 2 > ld% - 1) THEN GOSUB 222
                    END IF
                    splithands(pc2, dc1) = splithands(pc2, dc1) * prob
                    ERASE df: ERASE f
                END IF
            444 NEXT pc2: pc2 = pc3: q = 0: IF pc1 <> 0 OR dsa% = 1 THEN mc(pc2) = mc(pc2) + mc(19): mc(10) = mc(10) + mc(19)
            IF pc1 <= pc2 THEN n = pc1 / 2 * (19 - pc1) + pc2 ELSE n = pc2 / 2 * (19 - pc2) + pc1
            IF pc1 = 0 AND dsa% <> 1 THEN
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
                q = 0: mc0 = mc(0): mc9 = mc(9): mc10 = mc(10)
                IF mc(pc2) < 1 THEN mc(19) = mc(pc2) ELSE mc(19) = 1
                IF mc(pc2) = 0 THEN GOTO 444 ELSE mc(pc2) = mc(pc2) - mc(19): mc(10) = mc(10) - mc(19)
                GOSUB 666
                FOR y = 0 TO 12
                dfar(n, dc1, y) = df(y): dsar(n, dc1, y) = df(y): NEXT y
                IF dc1 = 0 AND push% = 1 THEN df(4) = df(4) + df(10): df(10) = 0
                IF dc1 = 9 AND ten% = 1 THEN df(4) = df(4) + df(10): df(10) = 0
                FOR y = 0 TO 9
                    FOR z = 0 TO 5
                        IF z = 5 THEN IF y = 9 AND bjsh% = 1 THEN q = q + f(0, y) * df(12) * blackjack ELSE q = q + f(0, y) * df(12)
                        IF z < 5 THEN IF z < y - 5 THEN IF y = 9 AND bjsh% = 1 THEN q = q + f(0, y) * df(z) * blackjack ELSE q = q + f(0, y) * df(z)
                        IF z < 5 THEN IF z > y - 5 THEN q = q - f(0, y) * df(z)
                        IF z < 4 THEN IF z = y - 5 THEN IF dwt% = 1 THEN q = q - f(0, y) * df(z)
                        IF z = 4 AND y = 9 THEN
                            IF immed% = 1 THEN
                                q = q + f(0, y) * df(4)
                                IF bjsh% = 1 THEN q = q + f(0, y) * df(4) * (blackjack - 1)
                            END IF
                            IF immed% <> 1 AND dwt% = 1 THEN q = q - f(0, y) * df(4)
                        END IF
                NEXT z: NEXT y
                ERASE df: ERASE f
            END IF
            IF pc1 <> 0 OR dsa% = 1 THEN
                FOR x = 0 TO 9
                    q = q + splithands(x, dc1)
                NEXT x
            END IF
            ERASE splithands: ERASE df: probs = 0
            FOR x = 0 TO 12
                df(x) = dbr(n, dc1, x)
            NEXT x
            IF dc1 = 0 AND push% = 1 THEN df(4) = df(4) + df(10): df(10) = 0
            IF dc1 = 9 AND ten% = 1 THEN df(4) = df(4) + df(10): df(10) = 0
            IF hole% <> 1 THEN q = pshc(0, 1) * q - df(10)
            IF hole% = 1 THEN q = pshc(0, 1) * (q - df(10))
            aq(n, dc1, 30) = q
            IF q > hit(pc1 * (21 - pc1) / 2, dc1) THEN hit(pc1 * (21 - pc1) / 2, dc1) = q: br(pc1 * (21 - pc1) / 2, dc1) = 30
            split = q: REM PRINT USING "##"; pc1 + 1; : PRINT " "; : PRINT USING "##"; pc1 + 1; : PRINT " split strategy yields a win minus loss expectation of"; : PRINT USING "##.####"; q
            333 FOR x = 0 TO 12
                df(x) = dbr(n, dc1, x)
            NEXT x
            hit(n, dc1) = hit(n, dc1) + insur
            CLS
            account# = 0
            PRINT "                          point values assigned to"
            PRINT "               player cards are"; pc1 + 1; "and"; pc2 + 1; "dealer card is"; dc1 + 1
            PRINT "                  given the user input deck composition,"
            PRINT "                    and user defined set of game rules,"
            PRINT "                 here are the dealer finish probabilities,"
            PRINT "     and expected value of the available player response strategies"
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
                IF ins% = 1 THEN IF dc1 = 0 THEN IF insur > 0 THEN PRINT "insurance against 21 yields a win minus loss expectation of";: PRINT USING "##.####"; insur
                PRINT "": account# = 0
                IF br(n, dc1) < 30 THEN PRINT MID$(o$, br(n, dc1) * 5 + 1, 5); " stand strategy is found to be the optimal strategy. "
                IF br(n, dc1) = 30 THEN PRINT USING "##"; pc1 + 1;: PRINT " ";: PRINT USING "##"; pc1 + 1;: PRINT " split strategy is found to be the optimal strategy. "
                IF br(n, dc1) = 31 THEN IF early% <> 1 THEN PRINT "L surrender strategy is found to be the optimal strategy. "
                IF br(n, dc1) = 31 THEN IF early% = 1 THEN PRINT "E surrender strategy is found to be the optimal strategy. "
                IF br(n, dc1) = 32 THEN PRINT "double down strategy is found to be the optimal strategy. "
            END IF
            v$ = files$ + "ev.txt"
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
456 ERASE df: split% = 0: insur = 0: NEXT m: NEXT l: NEXT k
total = 0
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
            IF mc(pc1) = 0 THEN GOTO 567 ELSE prob(n, dc1) = mc(pc1) / mc(10): mc(pc1) = mc(pc1) - mc(19): mc(10) = mc(10) - mc(19)
            IF mc(pc2) < 1 THEN mc(19) = mc(pc2) ELSE mc(19) = 1
            IF mc(pc2) = 0 THEN GOTO 567 ELSE prob(n, dc1) = prob(n, dc1) * mc(pc2) / mc(10): mc(pc2) = mc(pc2) - mc(19): mc(10) = mc(10) - mc(19)
            IF pc1 <> pc2 THEN prob(n, dc1) = prob(n, dc1) * 2
            puc(n) = prob(n, dc1)
            IF mc(dc1) < 1 THEN mc(19) = mc(dc1) ELSE mc(19) = 1
            IF mc(dc1) = 0 THEN GOTO 567 ELSE prob(n, dc1) = prob(n, dc1) * mc(dc1) / mc(10): mc(dc1) = mc(dc1) - mc(19): mc(10) = mc(10) - mc(19)
            total = total + hit(n, dc1) * prob(n, dc1): probs = probs + prob(n, dc1)
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
sd.ms678 = ms678: sd.ss678 = ss678: sd.sp678 = sp678: sd.dd = dd%
sd.dd3 = dd(3): sd.dd4 = dd(4): sd.dd5 = dd(5): sd.dd6 = dd(6): sd.dd7 = dd(7)
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
v$ = files$ + "p.dat"
w = 0
OPEN v$ FOR RANDOM AS #1 LEN = LEN(database)
FOR x = 0 TO 54
    FOR y = 0 TO 9
        w = w + 1
        database.dat = prob(x, y)
        PUT #1, w, database
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
v$ = files$ + "e.dat"
w = 0
OPEN v$ FOR RANDOM AS #1 LEN = LEN(database)
FOR x = 0 TO 54
    FOR y = 0 TO 9
        FOR z = 0 TO 12
            w = w + 1
            database.dat = dbr(x, y, z)
            PUT #1, w, database
NEXT z: NEXT y: NEXT x
CLOSE #1
v$ = files$ + "q.dat"
w = 0
OPEN v$ FOR RANDOM AS #1 LEN = LEN(database)
FOR x = 0 TO 54
    FOR y = 0 TO 9
        FOR r = 0 TO 32
            FOR f = 0 TO 24
                w = w + 1
                database.dat = pbr(x, y, r, f)
                PUT #1, w, database
NEXT f: NEXT r: NEXT y: NEXT x
CLOSE #1
v$ = files$ + "f.dat"
w = 0
OPEN v$ FOR RANDOM AS #1 LEN = LEN(database)
FOR x = 0 TO 54
    FOR y = 0 TO 9
        FOR z = 0 TO 12
            w = w + 1
            database.dat = dfar(x, y, z)
            PUT #1, w, database
NEXT z: NEXT y: NEXT x
CLOSE #1
v$ = files$ + "r.dat"
w = 0
OPEN v$ FOR RANDOM AS #1 LEN = LEN(database)
FOR x = 0 TO 54
    FOR y = 0 TO 9
        FOR r = 0 TO 32
            FOR f = 0 TO 24
                w = w + 1
                database.dat = pfar(x, y, r, f)
                PUT #1, w, database
NEXT f: NEXT r: NEXT y: NEXT x
CLOSE #1
v$ = files$ + "g.dat"
w = 0
OPEN v$ FOR RANDOM AS #1 LEN = LEN(database)
FOR x = 0 TO 54
    FOR y = 0 TO 9
        FOR z = 0 TO 12
            w = w + 1
            database.dat = dsar(x, y, z)
            PUT #1, w, database
NEXT z: NEXT y: NEXT x
CLOSE #1
v$ = files$ + "s.dat"
w = 0
OPEN v$ FOR RANDOM AS #1 LEN = LEN(database)
FOR x = 0 TO 54
    FOR y = 0 TO 9
        FOR r = 0 TO 32
            FOR f = 0 TO 24
                w = w + 1
                database.dat = psar(x, y, r, f)
                PUT #1, w, database
NEXT f: NEXT r: NEXT y: NEXT x
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
                        IF w * x = 0 AND w + x < h(br(n, y - 1)) THEN PRINT #1, MID$(o$, br(n, y - 1) * 5 + 1, 5); "  ";
                    END IF
                END IF
                IF w + x >= h(br(n, y - 1)) THEN
                    IF w * x <> 0 THEN
                        IF w + x + 2 >= g(br(n, y - 1)) THEN PRINT #1, "Stand  ";
                        IF w * x <> 0 AND w + x + 2 < g(br(n, y - 1)) THEN PRINT #1, MID$(n$, br(n, y - 1) * 5 + 1, 5); "  ";
                    END IF
                    IF w * x = 0 THEN
                        IF w + x >= h(br(n, y - 1)) THEN PRINT #1, "Stand  ";
                        IF w * x = 0 AND w + x < h(br(n, y - 1)) THEN PRINT #1, MID$(n$, br(n, y - 1) * 5 + 1, 5); "  ";
                    END IF
                END IF
            END IF
NEXT y: PRINT #1, " ": NEXT x: NEXT w
PRINT #1, " "
PRINT #1, "win minus loss results for two card combos verses dealer up cards "
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
PRINT #1, "total win minus loss result is ";: PRINT #1, USING "##.###"; total * 100;: PRINT #1, "%"
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
                        IF w * x <> 0 AND w + x + 2 < g(far(n, y - 1)) THEN PRINT #1, MID$(o$, far(n, y - 1) * 5 + 1, 5);
                    END IF
                    IF w * x = 0 THEN
                        IF w + x >= h(far(n, y - 1)) THEN PRINT #1, "Stand";
                        IF w * x = 0 AND w + x < h(far(n, y - 1)) THEN PRINT #1, MID$(o$, far(n, y - 1) * 5 + 1, 5);
                    END IF
                    IF far(n, y - 1) <> br(n, y - 1) THEN PRINT #1, "* ";
                    IF far(n, y - 1) = br(n, y - 1) THEN PRINT #1, "  ";
                END IF
                IF w + x >= h(far(n, y - 1)) THEN
                    IF w * x <> 0 THEN
                        IF w + x + 2 >= g(far(n, y - 1)) THEN PRINT #1, "Stand";
                        IF w * x <> 0 AND w + x + 2 < g(far(n, y - 1)) THEN PRINT #1, MID$(n$, far(n, y - 1) * 5 + 1, 5);
                    END IF
                    IF w * x = 0 THEN
                        IF w + x >= h(far(n, y - 1)) THEN PRINT #1, "Stand";
                        IF w * x = 0 AND w + x < h(far(n, y - 1)) THEN PRINT #1, MID$(n$, far(n, y - 1) * 5 + 1, 5);
                    END IF
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
                        IF w * x <> 0 AND w + x + 2 < g(sar(n, y - 1)) THEN PRINT #1, MID$(o$, sar(n, y - 1) * 5 + 1, 5);
                    END IF
                    IF w * x = 0 THEN
                        IF w + x >= h(sar(n, y - 1)) THEN PRINT #1, "Stand";
                        IF w * x = 0 AND w + x < h(sar(n, y - 1)) THEN PRINT #1, MID$(o$, sar(n, y - 1) * 5 + 1, 5);
                    END IF
                    IF sar(n, y - 1) <> br(n, y - 1) THEN PRINT #1, "* ";
                    IF sar(n, y - 1) = br(n, y - 1) THEN PRINT #1, "  ";
                END IF
                IF w + x >= h(sar(n, y - 1)) THEN
                    IF w * x <> 0 THEN
                        IF w + x + 2 >= g(sar(n, y - 1)) THEN PRINT #1, "Stand";
                        IF w * x <> 0 AND w + x + 2 < g(sar(n, y - 1)) THEN PRINT #1, MID$(n$, sar(n, y - 1) * 5 + 1, 5);
                    END IF
                    IF w * x = 0 THEN
                        IF w + x >= h(sar(n, y - 1)) THEN PRINT #1, "Stand";
                        IF w * x = 0 AND w + x < h(sar(n, y - 1)) THEN PRINT #1, MID$(n$, sar(n, y - 1) * 5 + 1, 5);
                    END IF
                    IF sar(n, y - 1) <> br(n, y - 1) THEN PRINT #1, "* ";
                    IF sar(n, y - 1) = br(n, y - 1) THEN PRINT #1, "  ";
                END IF
            END IF
NEXT y: PRINT #1, " ": NEXT x: NEXT w
CLOSE #1: END

