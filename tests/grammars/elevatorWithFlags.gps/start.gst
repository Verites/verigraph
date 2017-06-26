<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<gxl xmlns="http://www.gupro.de/GXL/gxl-1.0.dtd">
    <graph role="graph" edgeids="false" edgemode="directed" id="start">
        <attr name="$version">
            <string>curly</string>
        </attr>
        <node id="n1">
            <attr name="layout">
                <string>66 187 60 17</string>
            </attr>
        </node>
        <node id="n2">
            <attr name="layout">
                <string>190 80 34 17</string>
            </attr>
        </node>
        <node id="n3">
            <attr name="layout">
                <string>197 184 34 17</string>
            </attr>
        </node>
        <node id="n0">
            <attr name="layout">
                <string>66 82 60 34</string>
            </attr>
        </node>
        <edge from="n1" to="n1">
            <attr name="label">
                <string>type:elevator</string>
            </attr>
        </edge>
        <edge from="n1" to="n3">
            <attr name="label">
                <string>on</string>
            </attr>
        </edge>
        <edge from="n2" to="n2">
            <attr name="label">
                <string>type:floor</string>
            </attr>
        </edge>
        <edge from="n3" to="n3">
            <attr name="label">
                <string>type:floor</string>
            </attr>
        </edge>
        <edge from="n3" to="n2">
            <attr name="label">
                <string>next_up</string>
            </attr>
        </edge>
        <edge from="n0" to="n0">
            <attr name="label">
                <string>type:upDown</string>
            </attr>
        </edge>
        <edge from="n0" to="n0">
            <attr name="label">
                <string>flag:up</string>
            </attr>
        </edge>
    </graph>
</gxl>
