'ULTIMA299.BAS is a software application that enables the user to
'program their computer to conduct abbreviated-combinatorial analysis
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

_TITLE "ULTIMA299.BAS"
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
TYPE xbase
    dat AS SINGLE
END TYPE
DIM database AS xbase: DIM ez AS rec: DIM sd AS bsrules: DIM num AS bs: DIM wow AS str: DIM mdc AS comp: DIM adc(14, 600) AS DOUBLE: DIM aq(54, 9, 32)
DIM ir(54, 9) AS short: DIM ifar(54, 9) AS short: DIM isar(54, 9) AS short: DIM g(32) AS INTEGER: DIM h(32) AS INTEGER: DIM splithands(10, 9) AS DOUBLE
DIM hit(54, 9): DIM sar(54, 9) AS INTEGER: DIM far(54, 9) AS INTEGER: DIM br(54, 9) AS INTEGER: DIM dmult(14) AS INTEGER: DIM dadd(14) AS INTEGER: DIM dp(14) AS DOUBLE
DIM pfar(54, 9, 32, 24): DIM dfar(54, 9, 11): DIM psar(54, 9, 32, 24): DIM dsar(54, 9, 11): DIM dbr(54, 9, 11): DIM pbr(54, 9, 32, 24)
DIM duc(10): DIM f(29, 24) AS DOUBLE: DIM mc(33) AS DOUBLE: DIM df(11) AS DOUBLE: DIM mv(5) AS INTEGER: DIM puc(54): DIM dd(3 TO 7) AS INTEGER: DIM mc21po(11): DIM card(10)
DIM cv(14): DIM dv(13) AS DOUBLE: DIM ev(13) AS DOUBLE: DIM fv(13) AS DOUBLE: DIM gv(14) AS DOUBLE: DIM bc(12) AS INTEGER
DIM pc1 AS INTEGER: DIM pc2 AS INTEGER: DIM dc1 AS INTEGER: GOTO 226
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
                        FOR aa = 0 TO 9 STEP 1
                            dmult(9) = dmult(8) * aa: dadd(9) = dadd(8) + aa
                            IF mc(aa) < 1 THEN mc(22) = mc(aa) ELSE mc(22) = 1
                            IF mc(aa) = 0 THEN GOTO 25 ELSE dp(9) = dp(8) * mc(aa) / mc(10): mc(aa) = mc(aa) - mc(22): mc(10) = mc(10) - mc(22)
                            IF dadd(9) > g - 10 THEN mv(1) = dadd(9) - 3: f(I, mv(1)) = f(I, mv(1)) + dp(9): mc(aa) = mc(aa) + mc(22): mc(10) = mc(10) + mc(22): GOTO 25
                            IF dmult(9) = 0 AND dadd(9) > h - 8 AND dadd(9) < 3 THEN mv(1) = dadd(9) + 7: f(I, mv(1)) = f(I, mv(1)) + dp(9): mc(aa) = mc(aa) + mc(22): mc(10) = mc(10) + mc(22): GOTO 25
                            FOR ab = 0 TO 9 STEP 1
                                dmult(10) = dmult(9) * ab: dadd(10) = dadd(9) + ab
                                IF mc(ab) < 1 THEN mc(23) = mc(ab) ELSE mc(23) = 1
                                IF mc(ab) = 0 THEN GOTO 26 ELSE dp(10) = dp(9) * mc(ab) / mc(10): mc(ab) = mc(ab) - mc(23): mc(10) = mc(10) - mc(23)
                                IF dadd(10) > g - 11 THEN mv(1) = dadd(10) - 2: f(I, mv(1)) = f(I, mv(1)) + dp(10) * dd%: mc(ab) = mc(ab) + mc(23): mc(10) = mc(10) + mc(23): GOTO 26
                                IF dmult(10) = 0 AND dadd(10) > h - 9 AND dadd(10) < 2 THEN mv(1) = dadd(10) + 8: f(I, mv(1)) = f(I, mv(1)) + dp(10) * dd%: mc(ab) = mc(ab) + mc(23): mc(10) = mc(10) + mc(23): GOTO 26
                                FOR ac = 0 TO 9 STEP 1
                                    dmult(11) = dmult(10) * ac: dadd(11) = dadd(10) + ac
                                    IF mc(ac) < 1 THEN mc(24) = mc(ac) ELSE mc(24) = 1
                                    IF mc(ac) = 0 THEN GOTO 27 ELSE dp(11) = dp(10) * mc(ac) / mc(10): mc(ac) = mc(ac) - mc(24): mc(10) = mc(10) - mc(24)
                                    IF dadd(11) > g - 12 THEN mv(1) = dadd(11) - 1: f(I, mv(1)) = f(I, mv(1)) + dp(11): mc(ac) = mc(ac) + mc(24): mc(10) = mc(10) + mc(24): GOTO 27
                                    IF dmult(11) = 0 AND dadd(11) > h - 10 AND dadd(11) < 1 THEN mv(1) = dadd(11) + 9: f(I, mv(1)) = f(I, mv(1)) + dp(11): mc(ac) = mc(ac) + mc(24): mc(10) = mc(10) + mc(24): GOTO 27
                                    FOR ad = 0 TO 9 STEP 1
                                        dmult(12) = dmult(11) * ad: dadd(12) = dadd(11) + ad
                                        IF mc(ad) < 1 THEN mc(28) = mc(ad) ELSE mc(28) = 1
                                        IF mc(ad) = 0 THEN GOTO 15 ELSE dp(12) = dp(11) * mc(ad) / mc(10): mc(ad) = mc(ad) - mc(28): mc(10) = mc(10) - mc(28)
                                        IF dadd(12) > g - 13 THEN mv(1) = dadd(12) - 0: f(I, mv(1)) = f(I, mv(1)) + dp(12): mc(ad) = mc(ad) + mc(28): mc(10) = mc(10) + mc(28): GOTO 15
                                        IF dmult(12) = 0 AND dadd(12) > h - 11 AND dadd(12) < 0 THEN mv(1) = dadd(12) + 10: f(I, mv(1)) = f(I, mv(1)) + dp(12): mc(ad) = mc(ad) + mc(28): mc(10) = mc(10) + mc(28): GOTO 15
                                        FOR ae = 0 TO 9 STEP 1
                                            dmult(13) = dmult(12) * ae: dadd(13) = dadd(12) + ae
                                            IF mc(ae) < 1 THEN mc(29) = mc(ae) ELSE mc(29) = 1
                                            IF mc(ae) = 0 THEN GOTO 16 ELSE dp(13) = dp(12) * mc(ae) / mc(10): mc(ae) = mc(ae) - mc(29): mc(10) = mc(10) - mc(29)
                                            IF dadd(13) > g - 14 THEN mv(1) = dadd(13) + 1: f(I, mv(1)) = f(I, mv(1)) + dp(13) * dd%: mc(ae) = mc(ae) + mc(29): mc(10) = mc(10) + mc(29): GOTO 16
                                            IF dmult(13) = 0 AND dadd(13) > h - 12 AND dadd(13) < -1 THEN mv(1) = dadd(13) + 11: f(I, mv(1)) = f(I, mv(1)) + dp(13) * dd%: mc(ae) = mc(ae) + mc(29): mc(10) = mc(10) + mc(29): GOTO 16
                                            FOR af = 0 TO 9 STEP 1
                                                dmult(14) = dmult(13) * af: dadd(14) = dadd(13) + af
                                                IF mc(af) < 1 THEN mc(30) = mc(af) ELSE mc(30) = 1
                                                IF mc(af) = 0 THEN GOTO 17 ELSE dp(14) = dp(13) * mc(af) / mc(10): mc(af) = mc(af) - mc(30): mc(10) = mc(10) - mc(30)
                                                IF dadd(14) > g - 15 THEN mv(1) = dadd(14) + 2: f(I, mv(1)) = f(I, mv(1)) + dp(14): mc(af) = mc(af) + mc(30): mc(10) = mc(10) + mc(30): GOTO 17
                                                IF dmult(14) = 0 AND dadd(14) > h - 13 AND dadd(14) < -2 THEN mv(1) = dadd(14) + 12: f(I, mv(1)) = f(I, mv(1)) + dp(14): mc(af) = mc(af) + mc(30): mc(10) = mc(10) + mc(30): GOTO 17
                                                mc(af) = mc(af) + mc(30): mc(10) = mc(10) + mc(30): dd% = 1
                                            17 NEXT af
                                            mc(ae) = mc(ae) + mc(29): mc(10) = mc(10) + mc(29): dd% = 1
                                        16 NEXT ae
                                        mc(ad) = mc(ad) + mc(28): mc(10) = mc(10) + mc(28): dd% = 1
                                    15 NEXT ad
                                    mc(ac) = mc(ac) + mc(24): mc(10) = mc(10) + mc(24): dd% = 1
                                27 NEXT ac
                                mc(ab) = mc(ab) + mc(23): mc(10) = mc(10) + mc(23): dd% = 1
                            26 NEXT ab
                            mc(aa) = mc(aa) + mc(22): mc(10) = mc(10) + mc(22): dd% = 1
                        25 NEXT aa
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
666 dmult(1) = dc1: dadd(1) = dc1
FOR r = 0 TO 9 STEP 1
    dmult(2) = dmult(1) * r: dadd(2) = dadd(1) + r
    IF mc(r) < 1 THEN mc(13) = mc(r) ELSE mc(13) = 1
    IF mc(r) = 0 THEN GOTO 28 ELSE dp(2) = mc(r) / mc(10): mc(r) = mc(r) - mc(13): mc(10) = mc(10) - mc(13)
    IF dadd(2) > 14 THEN mv(1) = dadd(2) - 15: df(mv(1)) = df(mv(1)) + dp(2): mc(r) = mc(r) + mc(13): mc(10) = mc(10) + mc(13): GOTO 28
    IF dmult(2) = 0 AND dadd(2) > j - 1 THEN mv(1) = dadd(2) - 5: df(mv(1)) = df(mv(1)) + dp(2): mc(r) = mc(r) + mc(13): mc(10) = mc(10) + mc(13): GOTO 28
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
                            FOR aa = 0 TO 9 STEP 1
                                dmult(9) = dmult(8) * aa: dadd(9) = dadd(8) + aa
                                IF mc(aa) < 1 THEN mc(25) = mc(aa) ELSE mc(25) = 1
                                IF mc(aa) = 0 THEN GOTO 35 ELSE dp(9) = dp(8) * mc(aa) / mc(10): mc(aa) = mc(aa) - mc(25): mc(10) = mc(10) - mc(25)
                                IF dadd(9) > 7 THEN mv(1) = dadd(9) - 8: df(mv(1)) = df(mv(1)) + dp(9): mc(aa) = mc(aa) + mc(25): mc(10) = mc(10) + mc(25): GOTO 35
                                IF dmult(9) = 0 AND dadd(9) > j - 8 AND dadd(9) < 3 THEN mv(1) = dadd(9) + 2: df(mv(1)) = df(mv(1)) + dp(9): mc(aa) = mc(aa) + mc(25): mc(10) = mc(10) + mc(25): GOTO 35
                                FOR ab = 0 TO 9 STEP 1
                                    dmult(10) = dmult(9) * ab: dadd(10) = dadd(9) + ab
                                    IF mc(ab) < 1 THEN mc(26) = mc(ab) ELSE mc(26) = 1
                                    IF mc(ab) = 0 THEN GOTO 36 ELSE dp(10) = dp(9) * mc(ab) / mc(10): mc(ab) = mc(ab) - mc(26): mc(10) = mc(10) - mc(26)
                                    IF dadd(10) > 6 THEN mv(1) = dadd(10) - 7: df(mv(1)) = df(mv(1)) + dp(10): mc(ab) = mc(ab) + mc(26): mc(10) = mc(10) + mc(26): GOTO 36
                                    IF dmult(10) = 0 AND dadd(10) > j - 9 AND dadd(10) < 2 THEN mv(1) = dadd(10) + 3: df(mv(1)) = df(mv(1)) + dp(10): mc(ab) = mc(ab) + mc(26): mc(10) = mc(10) + mc(26): GOTO 36
                                    FOR ac = 0 TO 9 STEP 1
                                        dmult(11) = dmult(10) * ac: dadd(11) = dadd(10) + ac
                                        IF mc(ac) < 1 THEN mc(27) = mc(ac) ELSE mc(27) = 1
                                        IF mc(ac) = 0 THEN GOTO 37 ELSE dp(11) = dp(10) * mc(ac) / mc(10): mc(ac) = mc(ac) - mc(27): mc(10) = mc(10) - mc(27)
                                        IF dadd(11) > 5 THEN mv(1) = dadd(11) - 6: df(mv(1)) = df(mv(1)) + dp(11): mc(ac) = mc(ac) + mc(27): mc(10) = mc(10) + mc(27): GOTO 37
                                        IF dmult(11) = 0 AND dadd(11) > j - 10 AND dadd(11) < 1 THEN mv(1) = dadd(11) + 4: df(mv(1)) = df(mv(1)) + dp(11): mc(ac) = mc(ac) + mc(27): mc(10) = mc(10) + mc(27): GOTO 37
                                        FOR ad = 0 TO 9 STEP 1
                                            dmult(12) = dmult(11) * ad: dadd(12) = dadd(11) + ad
                                            IF mc(ad) < 1 THEN mc(31) = mc(ad) ELSE mc(31) = 1
                                            IF mc(ad) = 0 THEN GOTO 38 ELSE dp(12) = dp(11) * mc(ad) / mc(10): mc(ad) = mc(ad) - mc(31): mc(10) = mc(10) - mc(31)
                                            IF dadd(12) > 4 THEN mv(1) = dadd(12) - 5: df(mv(1)) = df(mv(1)) + dp(12): mc(ad) = mc(ad) + mc(31): mc(10) = mc(10) + mc(31): GOTO 38
                                            IF dmult(12) = 0 AND dadd(12) > j - 11 AND dadd(12) < 0 THEN mv(1) = dadd(12) + 5: df(mv(1)) = df(mv(1)) + dp(12): mc(ad) = mc(ad) + mc(31): mc(10) = mc(10) + mc(31): GOTO 38
                                            FOR ae = 0 TO 9 STEP 1
                                                dmult(13) = dmult(12) * ae: dadd(13) = dadd(9) + ae
                                                IF mc(ae) < 1 THEN mc(32) = mc(ae) ELSE mc(32) = 1
                                                IF mc(ae) = 0 THEN GOTO 39 ELSE dp(13) = dp(12) * mc(ae) / mc(10): mc(ae) = mc(ae) - mc(32): mc(10) = mc(10) - mc(32)
                                                IF dadd(13) > 3 THEN mv(1) = dadd(13) - 4: df(mv(1)) = df(mv(1)) + dp(13): mc(ae) = mc(ae) + mc(32): mc(10) = mc(10) + mc(32): GOTO 39
                                                IF dmult(13) = 0 AND dadd(13) > j - 12 AND dadd(13) < -1 THEN mv(1) = dadd(13) + 6: df(mv(1)) = df(mv(1)) + dp(13): mc(ae) = mc(ae) + mc(32): mc(10) = mc(10) + mc(32): GOTO 39
                                                FOR af = 0 TO 9 STEP 1
                                                    dmult(14) = dmult(13) * af: dadd(14) = dadd(13) + af
                                                    IF mc(af) < 1 THEN mc(33) = mc(af) ELSE mc(33) = 1
                                                    IF mc(af) = 0 THEN GOTO 40 ELSE dp(14) = dp(13) * mc(af) / mc(10): mc(af) = mc(af) - mc(33): mc(10) = mc(10) - mc(33)
                                                    IF dadd(14) > 2 THEN mv(1) = dadd(14) - 3: df(mv(1)) = df(mv(1)) + dp(14): mc(af) = mc(af) + mc(33): mc(10) = mc(10) + mc(33): GOTO 40
                                                    IF dmult(14) = 0 AND dadd(14) > j - 13 AND dadd(14) < -2 THEN mv(1) = dadd(14) + 7: df(mv(1)) = df(mv(1)) + dp(14): mc(af) = mc(af) + mc(33): mc(10) = mc(10) + mc(33): GOTO 40
                                                    mc(af) = mc(af) + mc(33): mc(10) = mc(10) + mc(33)
                                                40 NEXT af
                                                mc(ae) = mc(ae) + mc(32): mc(10) = mc(10) + mc(32)
                                            39 NEXT ae
                                            mc(ad) = mc(ad) + mc(31): mc(10) = mc(10) + mc(31)
                                        38 NEXT ad
                                        mc(ac) = mc(ac) + mc(27): mc(10) = mc(10) + mc(27)
                                    37 NEXT ac
                                    mc(ab) = mc(ab) + mc(26): mc(10) = mc(10) + mc(26)
                                36 NEXT ab
                                mc(aa) = mc(aa) + mc(25): mc(10) = mc(10) + mc(25)
                            35 NEXT aa
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
FOR y = 0 TO 9
df(10) = df(10) + df(y): NEXT y
FOR y = 1 TO 5
df(11) = df(11) + df(4 + y): NEXT y
IF push% <> 1 THEN
    IF dc1 = 0 THEN df(4) = df(4) - mc(9) / mc(10)
    FOR x = 0 TO 11
        IF x = 10 THEN x = 11
        IF dc1 = 0 THEN df(x) = df(x) / (1 - mc(9) / mc(10))
    NEXT x
END IF
IF ten% <> 1 THEN
    IF dc1 = 9 THEN df(4) = df(4) - mc(0) / mc(10)
    FOR x = 0 TO 11
        IF x = 10 THEN x = 11
        IF dc1 = 9 THEN df(x) = df(x) / (1 - mc(0) / mc(10))
    NEXT x
END IF
RETURN
888 IF pc1 <= pc2 THEN n = pc1 / 2 * (19 - pc1) + pc2 ELSE n = pc2 / 2 * (19 - pc2) + pc1
x = 0
IF split% <> 1 THEN hit(n, dc1) = -3 ELSE splithands(pc2, dc1) = -3
FOR v = 12 TO 19
    FOR w = 5 TO 8
        IF w + 12 < v THEN w = v - 12
        x = x + 1
        IF v >= hl% AND v <= hu% AND w + 12 >= sl% AND w + 12 <= su% THEN
            q# = 0
            IF bjsh% <> 1 THEN f(x, 9) = f(x, 9) + f(x, 17): f(x, 17) = 0
            IF po777 = 0 THEN f(x, 9) = f(x, 9) + f(x, 18): f(x, 18) = 0
            IF po678 = 0 THEN f(x, 9) = f(x, 9) + f(x, 19): f(x, 19) = 0
            IF mc21po(4) = 0 THEN f(x, 9) = f(x, 9) + f(x, 20): f(x, 20) = 0
            IF mc21po(5) = 0 THEN f(x, 9) = f(x, 9) + f(x, 21): f(x, 21) = 0
            IF mc21po(6) = 0 THEN f(x, 9) = f(x, 9) + f(x, 22): f(x, 22) = 0
            IF mc21po(7) = 0 THEN f(x, 9) = f(x, 9) + f(x, 23): f(x, 23) = 0
            FOR y = 0 TO 16
                IF y > 9 THEN
                    FOR z = 0 TO 4
                        q# = q# - f(x, y) * df(z)
                    NEXT z
                    FOR z = 1 TO 5
                        IF dbc% <> 1 AND dbc% <> 2 THEN q# = q# - f(x, y) * df(4 + z)
                        IF dbc% = 1 OR dbc% = 2 THEN
                            IF z + 21 < y + 12 THEN q# = q# - f(x, y) * df(4 + z)
                            IF z + 21 = y + 12 THEN
                                IF dwt% = 1 THEN q# = q# - f(x, y) * df(4 + z)
                                IF dwt% <> 1 AND dbc% = 1 THEN q# = q# - f(x, y) * df(4 + z) * .5
                            END IF
                            IF z + 21 > y + 12 AND dbc% = 2 THEN q# = q# + f(x, y) * df(4 + z)
                        END IF
                    NEXT z
                END IF
                IF y < 10 THEN
                    FOR z = 0 TO 5
                        IF z = 5 THEN q# = q# + f(x, y) * df(11)
                        IF z < 5 THEN IF z < y - 5 THEN q# = q# + f(x, y) * df(z)
                        IF z < 5 THEN IF z > y - 5 THEN q# = q# - f(x, y) * df(z)
                        IF z < 4 THEN IF z = y - 5 THEN IF dwt% = 1 THEN q# = q# - f(x, y) * df(z)
                        IF z = 4 AND y = 9 THEN
                            IF immed% = 1 THEN q# = q# + f(x, y) * df(4)
                            IF immed% <> 1 AND dwt% = 1 THEN q# = q# - f(x, y) * df(4)
                        END IF
                    NEXT z
                END IF
            NEXT y
            q# = q# + f(x, 17) * blackjack + f(x, 18) * (po777 + 1) + f(x, 19) * (po678 + 1) + f(x, 20) * (mc21po(4) + 1) + f(x, 21) * (mc21po(5) + 1) + f(x, 22) * (mc21po(6) + 1) + f(x, 23) * (mc21po(7) + 1) + f(x, 24)
            IF split% <> 1 THEN
                IF dc1 = 0 AND push% <> 1 THEN q# = q# * (1 - mc9 / mc10) - mc9 / mc10
                IF dc1 = 9 AND ten% <> 1 THEN q# = q# * (1 - mc0 / mc10) - mc0 / mc10
                aq(n, dc1, x) = q#
                IF q# > hit(n, dc1) THEN hit(n, dc1) = q#: br(n, dc1) = x
            END IF
            IF split% = 1 THEN
                IF pc2 < pc1 THEN IF q# > splithands(pc2, dc1) THEN splithands(pc2, dc1) = q#: far(n, dc1) = x
                IF pc2 = pc1 THEN IF q# > splithands(pc2, dc1) THEN splithands(pc2, dc1) = q#: far(n, dc1) = x: sar(n, dc1) = x
                IF pc1 < pc2 THEN IF q# > splithands(pc2, dc1) THEN splithands(pc2, dc1) = q#: sar(n, dc1) = x
            END IF
            IF pc1 * pc2 = 0 THEN
                IF pc1 + pc2 > 5 AND w < 6 THEN IF x < 25 THEN x = x + 1: w = w + 1: IF split% <> 1 THEN aq(n, dc1, x) = q#
                IF pc1 + pc2 > 6 AND w < 7 THEN IF x < 28 THEN x = x + 1: w = w + 1: IF split% <> 1 THEN aq(n, dc1, x) = q#
                IF pc1 + pc2 > 7 AND w < 8 THEN x = x + 1: w = w + 1: IF split% <> 1 THEN aq(n, dc1, x) = q#
            END IF
            IF pc1 * pc2 <> 0 THEN
                IF pc1 + pc2 > 4 AND w < 6 THEN IF x < 25 THEN x = x + 1: w = w + 1: IF split% <> 1 THEN aq(n, dc1, x) = q#
                IF pc1 + pc2 > 5 AND w < 7 THEN IF x < 28 THEN x = x + 1: w = w + 1: IF split% <> 1 THEN aq(n, dc1, x) = q#
                IF pc1 + pc2 > 6 AND w < 8 THEN x = x + 1: w = w + 1: IF split% <> 1 THEN aq(n, dc1, x) = q#
            END IF
        END IF
NEXT w: NEXT v: account = 0: q# = 0
IF split% <> 1 THEN standr% = br(n, dc1): stand = hit(n, dc1): REM PRINT MID$(o$, br(n, dc1) * 5 + 1, 5); " stand strategy yields a win minus loss expectation of"; : PRINT USING "##.####"; hit(n, dc1)
RETURN
222 FOR x = 0 TO 9
    dadd(0) = pc1 + pc2 + x: dmult(0) = pc1 * pc2 * x: IF dadd(0) > 26 THEN dadd(0) = 26
    IF dmult(0) <> 0 THEN IF dadd(0) > 8 THEN f(0, dadd(0) - 9) = f(0, dadd(0) - 9) + mc(x) / mc(10) ELSE f(0, 4) = f(0, 4) + mc(x) / mc(10)
    IF dmult(0) = 0 THEN IF dadd(0) > 3 THEN IF dadd(0) < 9 THEN f(0, dadd(0) + 1) = f(0, dadd(0) + 1) + mc(x) / mc(10) ELSE f(0, dadd(0) - 9) = f(0, dadd(0) - 9) + mc(x) / mc(10)
    IF dmult(0) = 0 THEN IF dadd(0) < 4 THEN f(0, 4) = f(0, 4) + mc(x) / mc(10)
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
            q# = q# - f(0, y) * df(z)
        NEXT z
        FOR z = 1 TO 5
            IF dbc% <> 1 AND dbc% <> 2 THEN q# = q# - f(0, y) * df(4 + z)
            IF dbc% = 1 OR dbc% = 2 THEN
                IF z + 21 < y + 12 THEN q# = q# - f(0, y) * df(4 + z)
                IF z + 21 = y + 12 THEN
                    IF dwt% = 1 THEN q# = q# - f(0, y) * df(4 + z)
                    IF dwt% <> 1 AND dbc% = 1 THEN q# = q# - f(0, y) * df(4 + z) * .5
                END IF
                IF z + 21 > y + 12 AND dbc% = 2 THEN q# = q# + f(0, y) * df(4 + z)
            END IF
        NEXT z
    END IF
    IF y < 10 THEN
        FOR z = 0 TO 5
            IF z = 5 THEN q# = q# + f(0, y) * df(11)
            IF z < 5 THEN IF z < y - 5 THEN q# = q# + f(0, y) * df(z)
            IF z < 5 THEN IF z > y - 5 THEN q# = q# - f(0, y) * df(z)
            IF z < 4 THEN IF z = y - 5 THEN IF dwt% = 1 THEN q# = q# - f(0, y) * df(z)
            IF z = 4 AND y = 9 THEN
                IF immed% = 1 THEN q# = q# + f(0, y) * df(4)
                IF immed% <> 1 AND dwt% = 1 THEN q# = q# - f(0, y) * df(4)
            END IF
        NEXT z
    END IF
NEXT y
IF (hole% <> 1 AND split% = 1) THEN q# = 2 * q#
IF split% <> 1 THEN
    IF hole% <> 1 THEN q# = 2 * q#
    IF dc1 = 0 THEN IF push% <> 1 THEN q# = (q# * (1 - mc9 / mc10) - mc9 / mc10) * (1 + hole%) ELSE IF hole% = 1 THEN q# = 2 * q#
    IF dc1 = 9 THEN IF ten% <> 1 THEN q# = (q# * (1 - mc0 / mc10) - mc0 / mc10) * (1 + hole%) ELSE IF hole% = 1 THEN q# = 2 * q#
    IF dc1 <> 0 AND dc1 <> 9 THEN IF hole% = 1 THEN q# = 2 * q#
    aq(n, dc1, 32) = q#
    IF q# > hit(n, dc1) THEN hit(n, dc1) = q#: br(n, dc1) = 32
    ddown = q#: REM PRINT "double down strategy yields a win minus loss expectation of"; : PRINT USING "##.####"; q#
END IF
IF split% = 1 THEN IF pc1 > pc2 THEN IF q# * prob# > splithands(pc2, dc1) THEN splithands(pc2, dc1) = q# * prob#: far(n, dc1) = 32
IF split% = 1 THEN IF pc2 = pc1 THEN IF q# * prob# > splithands(pc2, dc1) THEN splithands(pc2, dc1) = q# * prob#: far(n, dc1) = 32: sar(n, dc1) = 32
IF split% = 1 THEN IF pc2 > pc1 THEN IF q# * prob# > splithands(pc2, dc1) THEN splithands(pc2, dc1) = q# * prob#: sar(n, dc1) = 32
RETURN
234 IF (split% <> 1 AND ls% = 1) THEN
    q# = -.5
    IF dc1 = 0 AND early% <> 1 THEN q# = q# * (1 - mc9 / mc10) - mc9 / mc10
    IF dc1 = 9 AND early% <> 1 THEN q# = q# * (1 - mc0 / mc10) - mc0 / mc10
    aq(n, dc1, 31) = q#
    IF q# > hit(n, dc1) THEN hit(n, dc1) = q#: br(n, dc1) = 31
    IF early% <> 1 THEN srndr = q#: REM PRINT "L surrender strategy yields a win minus loss expectation of"; : PRINT USING "##.####"; q#
    IF early% = 1 THEN srndr = q#: REM PRINT "E surrender strategy yields a win minus loss expectation of"; : PRINT USING "##.####"; q#
END IF
RETURN
226 CLS: PRINT "ULTIMA299.BAS loads data saved by ULTIMA282.BAS about playing strategy."
PRINT "ULTIMA299.BAS loads data saved by ULTIMA298.BAS about average stack compositions"
PRINT "that occur at various card count totals during a simulated game of blackjack"
PRINT "and generates indices. ULTIMA297.BAS loads and uses these indices to vary"
PRINT "player response strategy with changes in the integer card count total during"
PRINT "a simulated game of blackjack. Card count values are assigned to each"
PRINT "denomination by the user. ULTIMA299.BAS uses these card count values to deduct"
PRINT "the card count value of the player's hand and dealer up from the total count."
PRINT "Doing so makes it possible to enable the computer player to include both the"
PRINT "player's hand and the dealers up card in the count to access play variation"
PRINT "indices. Card count values should only be input when a Hi-Lo type system is"
PRINT "being tested. The user should enter zeros for card count values whenever a"
PRINT "OPP type system is being tested."
PRINT ""
PRINT "enter 'folder/filename prefix' to be used to store the input screen and program output": INPUT files$
OPEN files$ + "i.txt" FOR OUTPUT AS #1
PRINT #1, "ULTIMA299.BAS loads data saved by ULTIMA282.BAS about playing strategy."
PRINT #1, "ULTIMA299.BAS loads data saved by ULTIMA298.BAS about average stack compositions"
PRINT #1, "that occur at various card count totals during a simulated game of blackjack"
PRINT #1, "and generates indices. ULTIMA297.BAS loads and uses these indices to vary"
PRINT #1, "player response strategy with changes in the integer card count total during"
PRINT #1, "a simulated game of blackjack. ULTIMA298.BAS stores the number of times bets"
PRINT #1, "were placed at the various integer card count totals that occurs during "
PRINT #1, "a simulated a game of blackjack. ULTIMA298.BAS also stores data about stack"
PRINT #1, "compositions that occur at the time a bet was placed at those various integer"
PRINT #1, "card count totals that occur during simulated play of a game of blackjack."
PRINT #1, "The average stack composition that occurred at any given card count total"
PRINT #1, "can be found by dividing the sum of all stack compositions found at a given"
PRINT #1, "integer card count total by the sum of all the times bets were placed at"
PRINT #1, "that card count total. ULTIMA299.BAS loads this data saved by ULTIMA298.BAS"
PRINT #1, "about the various integer card count totals that occurred during"
PRINT #1, "a simulated game of blackjack and about the average stack compositions"
PRINT #1, "that correlate with those various integer card count totals and does"
PRINT #1, "an approximate combinatorial analysis to find a nearly optimal"
PRINT #1, "combination dependent playing strategy for each of the average stack"
PRINT #1, "compositions that occured at each of the various integer card count levels."
PRINT #1, "This approximate combinatorial analysis of average deck compositions"
PRINT #1, "is the basis for the playing indices used by ULTIMA297.BAS to"
PRINT #1, "vary playing strategy with changes in the integer card count total"
PRINT #1, "during a user defined simulated game of blackjack. These three programs"
PRINT #1, "when used together offer a way of creating and testing playing indices"
PRINT #1, "for a user defined card count strategy in a simulated game of blackjack."
PRINT #1, ""
PRINT #1, "enter 'folder/filename prefix' to be used to store the input screen and program output ": PRINT #1, files$
CLOSE #1
o$ = "     12/A612/A712/A812/A913/A613/A713/A813/A914/A614/A714/A814/A915/A615/A715/A815/A916/A616/A716/A816/A917/A617/A717/A817/A918/A718/A818/A919/A819/A9SplitSrndrDDown"
n$ = "     12   12   12   12   13   13   13   13   14   14   14   14   15   15   15   15   16   16   16   16   17   17   17   17   18   18   18   19   19   SplitSrndrDDown"
69 PRINT "enter 'folder/filename prefix' of matching basic strategy": INPUT D$
OPEN D$ + "a.dat" FOR RANDOM AS #1 LEN = LEN(sd)
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
q$ = sd.description
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
OPEN D$ + "b.dat" FOR RANDOM AS #1 LEN = LEN(num)
w = 0
FOR x = 0 TO 54
    FOR y = 0 TO 9
        w = w + 1
        GET #1, w, num
        br(x, y) = num.r: far(x, y) = num.far: sar(x, y) = num.sar: hit(x, y) = num.hit
NEXT y: NEXT x
CLOSE #1
PRINT "strategy loaded is described as follows: "; q$
PRINT "if this is acceptable type 'y'and press enter ";: INPUT ans$: IF ans$ <> "y" THEN GOTO 69
IF rsa% = 1 THEN rsa% = splt% ELSE rsa% = 2
OPEN files$ + "i.txt" FOR APPEND AS #1
PRINT #1, "enter 'folder/filename prefix'of matching basic strategy ": PRINT #1, D$
PRINT #1, "strategy loaded is described as follows: ";: PRINT #1, q$
PRINT #1, "if this is acceptable type 'y'and press enter ";: PRINT #1, ans$
CLOSE #1
OPEN files$ + "i.txt" FOR APPEND AS #1
PRINT #1, "input the number of decks used";: PRINT #1, deck%
PRINT #1, "what is the percentage of the shoe remaining at point of reshuffle.(15-100)";: PRINT #1, shf
PRINT #1, "input dealer soft finish";: PRINT #1, j + 12
PRINT #1, "press 1 and enter for dealer wins ties";: PRINT #1, dwt%
PRINT #1, "press 1 and enter for a double bust contest where in if the player"
PRINT #1, "finishes closer to twenty-one than the dealer, the player pushes"
PRINT #1, "press 2 and enter for a double bust contest where in if the player"
PRINT #1, "finishes closer to twenty-one than the dealer, the player wins";: PRINT #1, dbc%
PRINT #1, "press 1 and enter for dealer takes no hole card";: PRINT #1, hole%
PRINT #1, "press 1 and enter for player hands of 21 push against all dealer bjs";: PRINT #1, push%
IF push% = 1 THEN ten% = 1 ELSE PRINT #1, "press 1 and enter for player 21 pushes against dealer 10 up bjs only";: PRINT #1, ten%
PRINT #1, "press 1 and enter for automatic six card player hand win";: PRINT #1, schw%
PRINT #1, "press 1 and enter for insurance";: PRINT #1, ins%
PRINT #1, "press 1 and enter for surrender";: PRINT #1, ls%
IF ls% = 1 THEN PRINT #1, "press 1 and enter for early surrender";: PRINT #1, early%
PRINT #1, "press 1 and enter for split option";: PRINT #1, splt%
IF splt% = 1 THEN
    PRINT #1, "press 1 and enter for double down after split";: PRINT #1, das%
    PRINT #1, "what is the maximum number of split hands allowed";: PRINT #1, most%
    IF most% > 2 THEN PRINT #1, "press 1 and enter for resplit of aces is allowed";: PRINT #1, rsa%
    PRINT #1, "press 1 and enter to draw to split aces";: PRINT #1, dsa%
END IF
PRINT #1, "press 1 and enter to restrict use of double down to specific hand totals";: PRINT #1, dh%
IF dh% = 1 THEN PRINT #1, "enter 9 for 9/10/11 only, enter 10 for 10/11 only, enter 11 for 11 only ";: PRINT #1, ld%
IF NOT dh% = 1 THEN ld% = 1
PRINT #1, "input 2 for blackjack pays 2:1 or 1.5 for pays 3:2 or 1 for pays 1:1";: PRINT #1, blackjack
PRINT #1, "press 1 and enter for blackjack pays off on split hands";: PRINT #1, bjsh%
PRINT #1, "press 1 and enter for all player hands of 21 pay off immediately";: PRINT #1, immed%
PRINT #1, "press 1 and enter for player blackjack beats dealer blackjack";: PRINT #1, pbd%
PRINT #1, "press 1 and enter for bonus 21 payoffs";: PRINT #1, b21po%
IF b21po% = 1 THEN
    FOR x = 5 TO 8
        PRINT #1, "enter payoff for"; x; " card 21";: PRINT #1, mc21po(x - 1)
    NEXT x
    PRINT #1, "enter payoff for 678 mixed suits";: PRINT #1, ms678
    PRINT #1, "enter payoff for 678 same suit";: PRINT #1, ss678
    PRINT #1, "enter payoff for 678 spades suit";: PRINT #1, sp678
    PRINT #1, "enter payoff for 777 mixed suits";: PRINT #1, ms777
    IF deck% > 2 THEN
        PRINT #1, "enter payoff for 777 same suit";: PRINT #1, ss777
        PRINT #1, "enter payoff for 777 spades suit";: PRINT #1, sp777
        PRINT #1, "enter jackpot for for 777 same suit against any dealer 7";: PRINT #1, jp777
    END IF
END IF
PRINT #1, "press 1 and enter for double on more than two cards": PRINT #1, dd%
IF dd% = 1 THEN
    FOR x = 3 TO 7
    PRINT #1, "press 1 and enter for double on "; x; " cards";: PRINT #1, dd(x): NEXT x
END IF
PRINT #1, "type and enter a brief description of rules": PRINT #1, q$
PRINT #1, ""
dd% = 1
CLOSE #1
70 PRINT "enter 'folder/filename prefix' of average deck compositions ": INPUT a$
OPEN a$ + "adc.dat" FOR RANDOM AS #1 LEN = LEN(mdc)
GET #1, , mdc
dc$ = mdc.description
CLOSE #1
PRINT "deck compositions loaded are described as follows:": PRINT dc$
PRINT "if this is acceptable type 'y'and press enter": INPUT ans$
OPEN files$ + "i.txt" FOR APPEND AS #1
PRINT #1, "enter 'folder/filename prefix' of average deck compositions ": PRINT #1, a$
PRINT #1, "deck compositions loaded are described as follows: ": PRINT #1, dc$
PRINT #1, "if this is acceptable type 'y'and press enter ": PRINT #1, ans$
CLOSE #1
IF ans$ <> "y" THEN GOTO 70
hmr& = mdc.hmr: ap% = mdc.ap
79 PRINT "press 'y' and enter to load an ezcount strategy ": INPUT ans$
OPEN files$ + "i.txt" FOR APPEND AS #1
PRINT #1, "press 'y' and enter to load an ezcount strategy ": PRINT #1, ans$
CLOSE #1
IF ans$ = "y" THEN
    PRINT "enter 'folder/filename prefix' of ezcount strategy to be loaded": INPUT z$
    OPEN z$ + "z.dat" FOR RANDOM AS #1 LEN = LEN(ez)
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
OPEN files$ + "i.txt" FOR APPEND AS #1
IF ans$ = "y" THEN
    PRINT #1, "enter 'folder/filename prefix' of ezcount strategy to be loaded ": PRINT #1, z$
    PRINT #1, "ezcount loaded is described as "; k$
END IF
CLOSE #1
IF ans$ = "y" THEN PRINT "if this is acceptable press 'y' and enter";: INPUT ans$: IF ans$ <> "y" THEN GOTO 79
OPEN files$ + "i.txt" FOR APPEND AS #1
IF ans$ = "y" THEN PRINT #1, "if this is acceptable press 'y' and enter ";: PRINT #1, ans$
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
808 cv(x) = c: dv(13) = dv(13) + cv(x) * mc(x) / 4: cv(13) = cv(13) - cv(x) * mc(x) * deck%: gv(14) = cv(14) + ABS(cv(x)) * mc(x) * deck%: NEXT x
OPEN files$ + "i.txt" FOR APPEND AS #1
IF ans$ <> "y" THEN
    PRINT #1, "press 1 and enter for default count values "; I
    FOR x = 0 TO 12
        PRINT #1, "what is the count value of ";: PRINT #1, MID$(b$, x + 1, 1);: PRINT #1, "'s ("; dv(x); ")":
    NEXT x
END IF
CLOSE #1
809 PRINT "press 1 and enter for true count conversion";: INPUT true%
PRINT "input a constant added to render the bet count";: INPUT constant
PRINT "Press 1 and enter to mathematically balance count strategy if unbalanced.";: INPUT bal%
IF bal% = 1 THEN
    PRINT "Press 1 and enter or 2 and enter to specify the type of balancing to be used. "
    PRINT "1 for additive, 2 for multiplicative": INPUT balance%
END IF
IF balance% = 1 THEN
    FOR x = 0 TO 12
        gv(x) = cv(x) + cv(13) / deck% / mc(13)
        cv(x) = gv(x)
    NEXT x
    cv(13) = 0
END IF
IF balance% = 2 THEN
    FOR x = 0 TO 12
        IF cv(x) >= 0 THEN gv(x) = cv(x) * cv(14) / (cv(14) + cv(13))
        IF cv(x) < 0 THEN gv(x) = cv(x) * cv(14) / (cv(14) - cv(13))
        cv(x) = gv(x)
    NEXT x
    cv(13) = 0: gv(13) = 0
END IF
FOR x = 9 TO 12 STEP 1
    cv(13) = cv(13) + bc(x) * cv(x)
NEXT x
cv(9) = cv(13) / (bc(9) + bc(10) + bc(11) + bc(12))
OPEN files$ + "i.txt" FOR APPEND AS #1
PRINT #1, "press 1 and enter for true count conversion ";: PRINT #1, true%
PRINT #1, "input a constant added to render the bet count ";: PRINT #1, constant
PRINT #1, "Press 1 and enter to mathematically balance count strategy if unbalanced. ";: PRINT #1, bal%
IF bal% = 1 THEN
    PRINT #1, "Press 1 or 2 and enter to specify the type of balancing to be used. "
    PRINT #1, "1 for additive, 2 for multiplicative ": PRINT #1, balance%
END IF
CLOSE #1
PRINT "input the minimum number of samples required to trigger this program"
PRINT "to produce an optimal playing strategy for an average deck composition"
PRINT "and use this in the process of creating playing indices.";: INPUT sample&
PRINT "input a description of indices": INPUT indx$
wow.description = indx$
OPEN files$ + "i.txt" FOR APPEND AS #1
PRINT #1, "input the minimum number of samples required to trigger this program"
PRINT #1, "to produce an optimal playing strategy for an average deck composition"
PRINT #1, "and use this in the process of creating playing indices. ";: PRINT #1, sample&
PRINT #1, "input a description of indices ";: PRINT #1, indx$
wow.description = indx$
CLOSE #1
I = 0
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
hl% = 12: hu% = 19: sl% = 17: su% = 20

OPEN files$ + "i.txt" FOR APPEND AS #1
PRINT #1, "input lower limit of player response strategies considered for hard hands ";: PRINT #1, hl%
PRINT #1, "input upper limit of player response strategies considered for hard hands ";: PRINT #1, hu%
PRINT #1, "input lower limit of player response strategies considered for soft hands ";: PRINT #1, sl%
PRINT #1, "input upper limit of player response strategies considered for soft hands ";: PRINT #1, su%
PRINT #1, ""
PRINT #1, ""
PRINT #1, "START: "; DATE$; " "; TIME$
CLOSE #1
OPEN a$ + "adc.dat" FOR RANDOM AS #1 LEN = LEN(mdc)
number = LOF(1) / LEN(mdc)
FOR x = 1 TO number
    GET #1, x, mdc
    adc(0, dcs) = mdc.adc0: adc(1, dcs) = mdc.adc1: adc(2, dcs) = mdc.adc2: adc(3, dcs) = mdc.adc3: adc(4, dcs) = mdc.adc4
    adc(5, dcs) = mdc.adc5: adc(6, dcs) = mdc.adc6: adc(7, dcs) = mdc.adc7: adc(8, dcs) = mdc.adc8: adc(9, dcs) = mdc.adc9
    adc(10, dcs) = mdc.adc10: adc(11, dcs) = mdc.adc11: adc(12, dcs) = mdc.adc12: adc(13, dcs) = mdc.adc13: adc(14, dcs) = mdc.adc14
    dcs = dcs + 1
NEXT x: CLOSE #1
z = 0
FOR x = 12 TO 19
    FOR y = 5 TO 8
        IF x > y + 12 THEN y = x - 12
        z = z + 1
        g(z) = x: h(z) = y
NEXT y: NEXT x: g(30) = 1000: g(31) = 1000: h(30) = 1000: h(31) = 1000: h(32) = 1000: g(32) = 1000
FOR level = 0 TO 600
    IF adc(14, level) < sample& THEN GOTO 949
    FOR x = 0 TO 13
        IF x < 10 THEN adc(x, level) = adc(x, level) / adc(14, level)
        IF x > 9 AND x < 13 THEN adc(9, level) = adc(9, level) + adc(x, level) / adc(14, level)
        IF x = 13 THEN adc(10, level) = adc(13, level) / adc(14, level)
    NEXT x
    FOR k = 9 TO 0 STEP -1
        FOR l = 9 TO k STEP -1
            n = k / 2 * (19 - k) + l
            FOR m = 9 TO 0 STEP -1
                pc1 = k: pc2 = l: dc1 = m
                IF adc(10, level) = 0 THEN GOTO 949
                FOR x = 0 TO 10
                mc(x) = adc(x, level): NEXT x
                FOR x = 0 TO 32
                aq(n, dc1, x) = -1: NEXT x
                IF mc(pc1) < 1 THEN mc(19) = mc(pc1) ELSE mc(19) = 1
                IF mc(pc1) = 0 THEN GOTO 456 ELSE mc(pc1) = mc(pc1) - mc(19): mc(10) = mc(10) - mc(19)
                IF mc(pc2) < 1 THEN mc(19) = mc(pc2) ELSE mc(19) = 1
                IF mc(pc2) = 0 THEN GOTO 456 ELSE mc(pc2) = mc(pc2) - mc(19): mc(10) = mc(10) - mc(19)
                IF mc(dc1) < 1 THEN mc(19) = mc(dc1) ELSE mc(19) = 1
                IF mc(dc1) = 0 THEN GOTO 456 ELSE mc(dc1) = mc(dc1) - mc(19): mc(10) = mc(10) - mc(19)
                mc0 = mc(0): mc9 = mc(9): mc10 = mc(10)
                IF ins% = 1 THEN IF dc1 = 0 THEN IF 3 * mc(9) / mc(10) > 1 THEN insur = 3 * mc(9) / mc(10) - 1 ELSE insur = 0
                IF (pc1 = 0 AND pc2 = 9) OR (pc1 = 9 AND pc2 = 0) THEN
                    q# = blackjack
                    IF pbd% <> 1 THEN
                        IF dc1 = 0 THEN q# = q# * (1 - mc(9) / mc(10)): IF dwt% = 1 THEN q# = q# - mc(9) / mc(10)
                        IF dc1 = 9 THEN q# = q# * (1 - mc(0) / mc(10)): IF dwt% = 1 THEN q# = q# - mc(0) / mc(10)
                    END IF
                    hit(9, dc1) = q#: br(9, dc1) = 1: far(9, dc1) = 1: sar(9, dc1) = 1: n = 9: aq(n, dc1, 1) = q#: GOTO 333
                END IF
                GOSUB 666
                FOR y = 0 TO 11
                dbr(n, dc1, y) = df(y): NEXT y
                FOR x = 0 TO 10
                mc(x) = adc(x, level): NEXT x
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
                        IF adc(10, level) = 0 THEN GOTO 444
                        FOR x = 0 TO 10
                        mc(x) = adc(x, level): NEXT x
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
                        FOR y = 0 TO 11
                        dfar(n, dc1, y) = df(y): NEXT y
                    END IF
                    IF pc1 = pc2 THEN
                        FOR y = 0 TO 11
                        dfar(n, dc1, y) = df(y): dsar(n, dc1, y) = df(y): NEXT y
                    END IF
                    IF pc2 > pc1 THEN
                        FOR y = 0 TO 11
                        dsar(n, dc1, y) = df(y): NEXT y
                    END IF
                    IF adc(10, level) = 0 THEN GOTO 444
                    FOR x = 0 TO 10
                    mc(x) = adc(x, level): NEXT x
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
                    x = 2: y = mc(pc1) / mc(10): anh = 2
                    mc(11) = mc(pc1): mc(20) = mc(10)
                    IF pc1 <> 0 OR rsa% = 1 THEN
                        DO WHILE x < most%
                            anh = anh + y * x: x = x + 1
                            IF mc(11) = 0 THEN x = most%
                            IF mc(11) < 1 THEN mc(19) = mc(11) ELSE mc(19) = 1
                            mc(11) = mc(11) - mc(19): mc(20) = mc(20) - mc(19) - 1
                            y = y * mc(11) / mc(20)
                        LOOP
                    END IF
                    IF mc(pc2) < 1 THEN mc(19) = mc(pc2) ELSE mc(19) = 1
                    IF mc(pc2) = 0 THEN GOTO 444
                    IF pc2 <> pc1 THEN prob# = mc(pc2) / mc(10) / (1 - mc(pc1) / mc(10) + y)
                    IF pc2 = pc1 THEN prob# = y / (1 - mc(pc1) / mc(10) + y)
                    IF pc1 = 0 AND dsa% <> 1 THEN dadd(0) = pc1 + pc2: f(0, dadd(0)) = f(0, dadd(0)) + prob#
                    mc(pc2) = mc(pc2) - mc(19): mc(10) = mc(10) - mc(19)
                    probs# = probs# + prob#
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
                        GOSUB 888: splithands(pc2, dc1) = splithands(pc2, dc1) * prob#
                        IF das% = 1 THEN
                            IF dsa% = 1 OR pc1 <> 0 THEN
                                IF dh% <> 1 THEN GOSUB 222 ELSE IF (pc1 + pc2 > ld% - 3 AND pc1 * pc2 <> 0) THEN GOSUB 222
                            END IF
                        END IF
                        ERASE df: ERASE f
                    END IF
                444 NEXT pc2: pc2 = pc3: q# = 0: IF pc1 <> 0 OR dsa% = 1 THEN mc(pc2) = mc(pc2) + mc(19): mc(10) = mc(10) + mc(19)
                IF pc1 <= pc2 THEN n = pc1 / 2 * (19 - pc1) + pc2 ELSE n = pc2 / 2 * (19 - pc2) + pc1
                IF pc1 = 0 AND dsa% <> 1 THEN
                    FOR x = 0 TO 10
                    mc(x) = adc(x, level): NEXT x
                    IF mc(pc1) < 1 THEN mc(19) = mc(pc1) ELSE mc(19) = 1
                    IF mc(pc1) = 0 THEN GOTO 456 ELSE mc(pc1) = mc(pc1) - mc(19): mc(10) = mc(10) - mc(19)
                    IF mc(pc1) < 1 THEN mc(19) = mc(pc1) ELSE mc(19) = 1
                    IF mc(pc1) = 0 THEN GOTO 456 ELSE mc(pc1) = mc(pc1) - mc(19): mc(10) = mc(10) - mc(19)
                    IF mc(dc1) < 1 THEN mc(19) = mc(dc1) ELSE mc(19) = 1
                    IF mc(dc1) = 0 THEN GOTO 456 ELSE mc(dc1) = mc(dc1) - mc(19): mc(10) = mc(10) - mc(19)
                    q# = 0: mc0 = mc(0): mc9 = mc(9): mc10 = mc(10)
                    IF mc(pc2) < 1 THEN mc(19) = mc(pc2) ELSE mc(19) = 1
                    IF mc(pc2) = 0 THEN GOTO 456 ELSE mc(pc2) = mc(pc2) - mc(19): mc(10) = mc(10) - mc(19)
                    GOSUB 666
                    FOR y = 0 TO 11
                    dfar(n, dc1, y) = df(y): dsar(n, dc1, y) = df(y): NEXT y
                    FOR y = 0 TO 9
                        FOR z = 0 TO 5
                            IF z = 5 THEN IF y = 9 AND bjsh% = 1 THEN q# = q# + f(0, y) * df(11) * blackjack ELSE q# = q# + f(0, y) * df(11)
                            IF z < 5 THEN IF z < y - 5 THEN IF y = 9 AND bjsh% = 1 THEN q# = q# + f(0, y) * df(z) * blackjack ELSE q# = q# + f(0, y) * df(z)
                            IF z < 5 THEN IF z > y - 5 THEN q# = q# - f(0, y) * df(z)
                            IF z < 4 THEN IF z = y - 5 THEN IF dwt% = 1 THEN q# = q# - f(0, y) * df(z)
                            IF z = 4 AND y = 9 THEN
                                IF immed% = 1 THEN
                                    q# = q# + f(0, y) * df(4)
                                    IF bjsh% = 1 THEN q# = q# + f(0, y) * df(4) * (blackjack - 1)
                                END IF
                                IF immed% <> 1 AND dwt% = 1 THEN q# = q# - f(0, y) * df(4)
                            END IF
                    NEXT z: NEXT y
                    ERASE df: ERASE f
                END IF
                IF pc1 <> 0 OR dsa% = 1 THEN
                    FOR x = 0 TO 9
                        q# = q# + splithands(x, dc1)
                    NEXT x
                END IF
                ERASE splithands: ERASE df: probs# = 0
                IF hole% <> 1 THEN q# = q# * anh
                IF dc1 = 0 THEN IF push% <> 1 THEN q# = (q# * (1 - mc9 / mc10) - mc9 / mc10) * (1 + hole%) ELSE IF hole% = 1 THEN q# = q# * anh
                IF dc1 = 9 THEN IF ten% <> 1 THEN q# = (q# * (1 - mc0 / mc10) - mc0 / mc10) * (1 + hole%) ELSE IF hole% = 1 THEN q# = q# * anh
                IF dc1 <> 0 AND dc1 <> 9 THEN IF hole% = 1 THEN q# = anh * q#
                aq(n, dc1, 30) = q#
                split = q#: IF q# > hit(pc1 * (21 - pc1) / 2, dc1) THEN hit(pc1 * (21 - pc1) / 2, dc1) = q#: br(pc1 * (21 - pc1) / 2, dc1) = 30
                333 FOR x = 0 TO 11
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
                PRINT " bust  "
                account# = account# + df(10)
                PRINT USING "####"; dc1 + 1;
                FOR y = 0 TO 5
                    IF y = 5 THEN y = 11
                    PRINT USING "####.###"; df(y) / (df(10) + 1E-08) * 100;
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
    456 ERASE df: split% = 0: insur = 0: NEXT m: NEXT l: NEXT k: GOSUB 999
    ERASE puc: ERASE f: ERASE df: ERASE splithands: ERASE br: ERASE far: ERASE sar: ERASE duc: ERASE mv: ERASE mc: r = 0
949 NEXT level
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
OPEN files$ + "i.txt" FOR APPEND AS #1
PRINT #1, ""
PRINT #1, "END:   "; DATE$; " "; TIME$
CLOSE #1
END
999 total# = 0
FOR k = 0 TO 9
    FOR l = k TO 9
        n = k / 2 * (19 - k) + l
        FOR m = 0 TO 9
            pc1 = k: pc2 = l: dc1 = m
            FOR x = 0 TO 10
            mc(x) = adc(x, level): NEXT x
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
FOR w = 0 TO 9
    FOR x = w TO 9
        n = w / 2 * (19 - w) + x
        FOR y = 0 TO 9
            cv% = cv(w) + cv(x) + cv(y): IF true% = 1 THEN cv% = CINT(cv% * card(10) / adc(10, level))
            IF br(n, y) > 0 THEN
                IF ir(n, y).nor = 9 THEN IF ir(n, y).nnr <> br(n, y) THEN ir(n, y).nor = 10: ir(n, y).nnl = cv% + level - 300: ir(n, y).lr = br(n, y)
                IF ir(n, y).nor = 8 THEN IF ir(n, y).er <> br(n, y) THEN ir(n, y).nor = 9: ir(n, y).el = cv% + level - 300: ir(n, y).nnr = br(n, y)
                IF ir(n, y).nor = 7 THEN IF ir(n, y).nr <> br(n, y) THEN ir(n, y).nor = 8: ir(n, y).nl = cv% + level - 300: ir(n, y).er = br(n, y)
                IF ir(n, y).nor = 6 THEN IF ir(n, y).xr <> br(n, y) THEN ir(n, y).nor = 7: ir(n, y).xl = cv% + level - 300: ir(n, y).nr = br(n, y)
                IF ir(n, y).nor = 5 THEN IF ir(n, y).vr <> br(n, y) THEN ir(n, y).nor = 6: ir(n, y).vl = cv% + level - 300: ir(n, y).xr = br(n, y)
                IF ir(n, y).nor = 4 THEN IF ir(n, y).rr <> br(n, y) THEN ir(n, y).nor = 5: ir(n, y).rl = cv% + level - 300: ir(n, y).vr = br(n, y)
                IF ir(n, y).nor = 3 THEN IF ir(n, y).tr <> br(n, y) THEN ir(n, y).nor = 4: ir(n, y).tl = cv% + level - 300: ir(n, y).rr = br(n, y)
                IF ir(n, y).nor = 2 THEN IF ir(n, y).sr <> br(n, y) THEN ir(n, y).nor = 3: ir(n, y).sl = cv% + level - 300: ir(n, y).tr = br(n, y)
                IF ir(n, y).nor = 1 THEN IF ir(n, y).fr <> br(n, y) THEN ir(n, y).nor = 2: ir(n, y).fl = cv% + level - 300: ir(n, y).sr = br(n, y)
                IF ir(n, y).nor = 0 THEN
                    ir(n, y).nor = 1: ir(n, y).fr = br(n, y): ir(n, y).fl = 1000: ir(n, y).sl = 1000: ir(n, y).tl = 1000: ir(n, y).rl = 1000
                    ir(n, y).vl = 1000: ir(n, y).xl = 1000: ir(n, y).nl = 1000: ir(n, y).el = 1000: ir(n, y).nnl = 1000: ir(n, y).ll = 1000
                END IF
            END IF
            IF far(n, y) > 0 THEN
                IF ifar(n, y).nor = 9 THEN IF ifar(n, y).nnr <> far(n, y) THEN ifar(n, y).nor = 10: ifar(n, y).nnl = cv% + level - 300: ifar(n, y).lr = far(n, y)
                IF ifar(n, y).nor = 8 THEN IF ifar(n, y).er <> far(n, y) THEN ifar(n, y).nor = 9: ifar(n, y).el = cv% + level - 300: ifar(n, y).nnr = far(n, y)
                IF ifar(n, y).nor = 7 THEN IF ifar(n, y).nr <> far(n, y) THEN ifar(n, y).nor = 8: ifar(n, y).nl = cv% + level - 300: ifar(n, y).er = far(n, y)
                IF ifar(n, y).nor = 6 THEN IF ifar(n, y).xr <> far(n, y) THEN ifar(n, y).nor = 7: ifar(n, y).xl = cv% + level - 300: ifar(n, y).nr = far(n, y)
                IF ifar(n, y).nor = 5 THEN IF ifar(n, y).vr <> far(n, y) THEN ifar(n, y).nor = 6: ifar(n, y).vl = cv% + level - 300: ifar(n, y).xr = far(n, y)
                IF ifar(n, y).nor = 4 THEN IF ifar(n, y).rr <> far(n, y) THEN ifar(n, y).nor = 5: ifar(n, y).rl = cv% + level - 300: ifar(n, y).vr = far(n, y)
                IF ifar(n, y).nor = 3 THEN IF ifar(n, y).tr <> far(n, y) THEN ifar(n, y).nor = 4: ifar(n, y).tl = cv% + level - 300: ifar(n, y).rr = far(n, y)
                IF ifar(n, y).nor = 2 THEN IF ifar(n, y).sr <> far(n, y) THEN ifar(n, y).nor = 3: ifar(n, y).sl = cv% + level - 300: ifar(n, y).tr = far(n, y)
                IF ifar(n, y).nor = 1 THEN IF ifar(n, y).fr <> far(n, y) THEN ifar(n, y).nor = 2: ifar(n, y).fl = cv% + level - 300: ifar(n, y).sr = far(n, y)
                IF ifar(n, y).nor = 0 THEN
                    ifar(n, y).nor = 1: ifar(n, y).fr = far(n, y): ifar(n, y).fl = 1000: ifar(n, y).sl = 1000: ifar(n, y).tl = 1000: ifar(n, y).rl = 1000
                    ifar(n, y).vl = 1000: ifar(n, y).xl = 1000: ifar(n, y).nl = 1000: ifar(n, y).el = 1000: ifar(n, y).nnl = 1000: ifar(n, y).ll = 1000
                END IF
            END IF
            IF sar(n, y) > 0 THEN
                IF isar(n, y).nor = 9 THEN IF isar(n, y).nnr <> sar(n, y) THEN isar(n, y).nor = 10: isar(n, y).nnl = cv% + level - 300: isar(n, y).lr = sar(n, y)
                IF isar(n, y).nor = 8 THEN IF isar(n, y).er <> sar(n, y) THEN isar(n, y).nor = 9: isar(n, y).el = cv% + level - 300: isar(n, y).nnr = sar(n, y)
                IF isar(n, y).nor = 7 THEN IF isar(n, y).nr <> sar(n, y) THEN isar(n, y).nor = 8: isar(n, y).nl = cv% + level - 300: isar(n, y).er = sar(n, y)
                IF isar(n, y).nor = 6 THEN IF isar(n, y).xr <> sar(n, y) THEN isar(n, y).nor = 7: isar(n, y).xl = cv% + level - 300: isar(n, y).nr = sar(n, y)
                IF isar(n, y).nor = 5 THEN IF isar(n, y).vr <> sar(n, y) THEN isar(n, y).nor = 6: isar(n, y).vl = cv% + level - 300: isar(n, y).xr = sar(n, y)
                IF isar(n, y).nor = 4 THEN IF isar(n, y).rr <> sar(n, y) THEN isar(n, y).nor = 5: isar(n, y).rl = cv% + level - 300: isar(n, y).vr = sar(n, y)
                IF isar(n, y).nor = 3 THEN IF isar(n, y).tr <> sar(n, y) THEN isar(n, y).nor = 4: isar(n, y).tl = cv% + level - 300: isar(n, y).rr = sar(n, y)
                IF isar(n, y).nor = 2 THEN IF isar(n, y).sr <> sar(n, y) THEN isar(n, y).nor = 3: isar(n, y).sl = cv% + level - 300: isar(n, y).tr = sar(n, y)
                IF isar(n, y).nor = 1 THEN IF isar(n, y).fr <> sar(n, y) THEN isar(n, y).nor = 2: isar(n, y).fl = cv% + level - 300: isar(n, y).sr = sar(n, y)
                IF isar(n, y).nor = 0 THEN
                    isar(n, y).nor = 1: isar(n, y).fr = sar(n, y): isar(n, y).fl = 1000: isar(n, y).sl = 1000: isar(n, y).tl = 1000: isar(n, y).rl = 1000
                    isar(n, y).vl = 1000: isar(n, y).xl = 1000: isar(n, y).nl = 1000: isar(n, y).el = 1000: isar(n, y).nnl = 1000: isar(n, y).ll = 1000
                END IF
            END IF
NEXT y: NEXT x: NEXT w
twentysixes% = INT((level) / 26): ones% = INT(level - twentysixes% * 26)
FOR x = 0 TO 10
mc(x) = adc(x, level): NEXT x
IF ss% = 1 THEN
    v$ = files$ + CHR$(twentysixes% + 97) + CHR$(ones% + 97) + "a.dat"
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
    IF LEN(q$) > 80 THEN sd.description = MID$(q$, 1, 80) ELSE sd.description = q$
    PUT #1, , sd
    CLOSE #1
    v$ = files$ + CHR$(twentysixes% + 97) + CHR$(ones% + 97) + "b.dat"
    w = 0
    OPEN v$ FOR RANDOM AS #1 LEN = LEN(num)
    FOR x = 0 TO 54
        FOR y = 0 TO 9
            w = w + 1
            num.far = far(x, y): num.sar = sar(x, y): num.r = br(x, y): num.hit = hit(x, y)
            PUT #1, w, num
    NEXT y: NEXT x
    CLOSE
END IF
v$ = files$ + CHR$(twentysixes% + 97) + CHR$(ones% + 97) + "t.dat"
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
v$ = files$ + CHR$(twentysixes% + 97) + CHR$(ones% + 97) + "e.dat"
w = 0
OPEN v$ FOR RANDOM AS #1 LEN = LEN(database)
FOR x = 0 TO 54
    FOR y = 0 TO 9
        FOR z = 0 TO 11
            w = w + 1
            database.dat = dbr(x, y, z)
            PUT #1, w, database
NEXT z: NEXT y: NEXT x
CLOSE #1
v$ = files$ + CHR$(twentysixes% + 97) + CHR$(ones% + 97) + "q.dat"
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
v$ = files$ + CHR$(twentysixes% + 97) + CHR$(ones% + 97) + "f.dat"
w = 0
OPEN v$ FOR RANDOM AS #1 LEN = LEN(database)
FOR x = 0 TO 54
    FOR y = 0 TO 9
        FOR z = 0 TO 11
            w = w + 1
            database.dat = dfar(x, y, z)
            PUT #1, w, database
NEXT z: NEXT y: NEXT x
CLOSE #1
v$ = files$ + CHR$(twentysixes% + 97) + CHR$(ones% + 97) + "r.dat"
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
v$ = files$ + CHR$(twentysixes% + 97) + CHR$(ones% + 97) + "g.dat"
w = 0
OPEN v$ FOR RANDOM AS #1 LEN = LEN(database)
FOR x = 0 TO 54
    FOR y = 0 TO 9
        FOR z = 0 TO 11
            w = w + 1
            database.dat = dsar(x, y, z)
            PUT #1, w, database
NEXT z: NEXT y: NEXT x
CLOSE #1
v$ = files$ + CHR$(twentysixes% + 97) + CHR$(ones% + 97) + "s.dat"
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
ERASE pbr: ERASE dbr: ERASE pfar: ERASE dfar: ERASE psar: ERASE dsar
FOR x = 0 TO 9
duc(x) = mc(x) / mc(10): NEXT x
IF sp% = 1 THEN
    v$ = files$ + CHR$(twentysixes% + 97) + CHR$(ones% + 97) + "o.txt"
    OPEN v$ FOR OUTPUT AS #1
    321 PRINT #1, "the most favorable player responses for level ";: PRINT #1, USING "####"; level - 300
    PRINT #1, "total number of times bets were placed is ";: PRINT #1, USING "###########"; adc(14, level);: PRINT #1, " hands"
    PRINT #1, q$
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
    PRINT #1, "total win minus loss result is ";: PRINT #1, USING "##.###"; total# * 100;: PRINT #1, "%"
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
END IF
RETURN
