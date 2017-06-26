<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<gxl xmlns="http://www.gupro.de/GXL/gxl-1.0.dtd">
    <graph role="graph" edgeids="false" edgemode="directed" id="start">
        <attr name="$version">
            <string>curly</string>
        </attr>
        <node id="n0">
            <attr name="layout">
                <string>260 158 40 17</string>
            </attr>
        </node>
        <node id="n1">
            <attr name="layout">
                <string>365 158 40 17</string>
            </attr>
        </node>
        <node id="n2">
            <attr name="layout">
                <string>257 223 40 17</string>
            </attr>
        </node>
        <node id="n3">
            <attr name="layout">
                <string>364 224 40 17</string>
            </attr>
        </node>
        <node id="n4">
            <attr name="layout">
                <string>246 296 59 17</string>
            </attr>
        </node>
        <node id="n5">
            <attr name="layout">
                <string>366 91 42 17</string>
            </attr>
        </node>
        <edge from="n0" to="n0">
            <attr name="label">
                <string>type:block</string>
            </attr>
        </edge>
        <edge from="n0" to="n1">
            <attr name="label">
                <string>next</string>
            </attr>
        </edge>
        <edge from="n0" to="n2">
            <attr name="label">
                <string>next</string>
            </attr>
        </edge>
        <edge from="n1" to="n1">
            <attr name="label">
                <string>type:block</string>
            </attr>
        </edge>
        <edge from="n1" to="n3">
            <attr name="label">
                <string>next</string>
            </attr>
        </edge>
        <edge from="n1" to="n0">
            <attr name="label">
                <string>next</string>
            </attr>
        </edge>
        <edge from="n2" to="n2">
            <attr name="label">
                <string>type:block</string>
            </attr>
        </edge>
        <edge from="n2" to="n0">
            <attr name="label">
                <string>next</string>
            </attr>
        </edge>
        <edge from="n2" to="n3">
            <attr name="label">
                <string>next</string>
            </attr>
        </edge>
        <edge from="n3" to="n3">
            <attr name="label">
                <string>type:block</string>
            </attr>
        </edge>
        <edge from="n3" to="n1">
            <attr name="label">
                <string>next</string>
            </attr>
        </edge>
        <edge from="n3" to="n2">
            <attr name="label">
                <string>next</string>
            </attr>
        </edge>
        <edge from="n4" to="n4">
            <attr name="label">
                <string>type:pacman</string>
            </attr>
        </edge>
        <edge from="n4" to="n2">
            <attr name="label">
                <string>in</string>
            </attr>
        </edge>
        <edge from="n5" to="n5">
            <attr name="label">
                <string>type:ghost</string>
            </attr>
        </edge>
        <edge from="n5" to="n1">
            <attr name="label">
                <string>in</string>
            </attr>
        </edge>
    </graph>
</gxl>
