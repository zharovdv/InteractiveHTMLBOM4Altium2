function fixtext(_text_, _font_) {
    var res = '';
    var chrArr = _text_.split("");
    var mk = chrArr.length;
    for (var i = 0; i < mk; i++) {
        if (!(chrArr[i] in _font_)) continue;
        res += chrArr[i];
        //if (chrArr[i] == "\t" && !Object.prototype.hasOwnProperty.call(res, chrArr[i])) {
        //    res[" "] = parseFontChar(" ");
        //}
        //if (!Object.prototype.hasOwnProperty.call(res, chrArr[i]) && chrArr[i].charCodeAt(0) > 31) {
        //    res[chrArr[i]] = parseFontChar(chrArr[i]);
        //}
    }
    return res;
}

function getfont() {
    // @formatter:off
    return {"0":{"l":[[[0.428571,-1.047619],[0.52381,-1.047619],[0.619048,-1],[0.666667,-0.952381],[0.714286,-0.857143],[0.761905,-0.666667],[0.761905,-0.428571],[0.714286,-0.238095],[0.666667,-0.142857],[0.619048,-0.095238],[0.52381,-0.047619],[0.428571,-0.047619],[0.333333,-0.095238],[0.285714,-0.142857],[0.238095,-0.238095],[0.190476,-0.428571],[0.190476,-0.666667],[0.238095,-0.857143],[0.285714,-0.952381],[0.333333,-1],[0.428571,-1.047619]]],"w":0.952381},"1":{"l":[[[0.761905,-0.047619],[0.190476,-0.047619]],[[0.47619,-0.047619],[0.47619,-1.047619],[0.380952,-0.904762],[0.285714,-0.809524],[0.190476,-0.761905]]],"w":0.952381},"2":{"l":[[[0.190476,-0.952381],[0.238095,-1],[0.333333,-1.047619],[0.571429,-1.047619],[0.666667,-1],[0.714286,-0.952381],[0.761905,-0.857143],[0.761905,-0.761905],[0.714286,-0.619048],[0.142857,-0.047619],[0.761905,-0.047619]]],"w":0.952381},"3":{"l":[[[0.142857,-1.047619],[0.761905,-1.047619],[0.428571,-0.666667],[0.571429,-0.666667],[0.666667,-0.619048],[0.714286,-0.571429],[0.761905,-0.47619],[0.761905,-0.238095],[0.714286,-0.142857],[0.666667,-0.095238],[0.571429,-0.047619],[0.285714,-0.047619],[0.190476,-0.095238],[0.142857,-0.142857]]],"w":0.952381},"4":{"l":[[[0.666667,-0.714286],[0.666667,-0.047619]],[[0.428571,-1.095238],[0.190476,-0.380952],[0.809524,-0.380952]]],"w":0.952381},"5":{"l":[[[0.714286,-1.047619],[0.238095,-1.047619],[0.190476,-0.571429],[0.238095,-0.619048],[0.333333,-0.666667],[0.571429,-0.666667],[0.666667,-0.619048],[0.714286,-0.571429],[0.761905,-0.47619],[0.761905,-0.238095],[0.714286,-0.142857],[0.666667,-0.095238],[0.571429,-0.047619],[0.333333,-0.047619],[0.238095,-0.095238],[0.190476,-0.142857]]],"w":0.952381},"6":{"l":[[[0.666667,-1.047619],[0.47619,-1.047619],[0.380952,-1],[0.333333,-0.952381],[0.238095,-0.809524],[0.190476,-0.619048],[0.190476,-0.238095],[0.238095,-0.142857],[0.285714,-0.095238],[0.380952,-0.047619],[0.571429,-0.047619],[0.666667,-0.095238],[0.714286,-0.142857],[0.761905,-0.238095],[0.761905,-0.47619],[0.714286,-0.571429],[0.666667,-0.619048],[0.571429,-0.666667],[0.380952,-0.666667],[0.285714,-0.619048],[0.238095,-0.571429],[0.190476,-0.47619]]],"w":0.952381},"7":{"l":[[[0.142857,-1.047619],[0.809524,-1.047619],[0.380952,-0.047619]]],"w":0.952381},"8":{"l":[[[0.380952,-0.619048],[0.285714,-0.666667],[0.238095,-0.714286],[0.190476,-0.809524],[0.190476,-0.857143],[0.238095,-0.952381],[0.285714,-1],[0.380952,-1.047619],[0.571429,-1.047619],[0.666667,-1],[0.714286,-0.952381],[0.761905,-0.857143],[0.761905,-0.809524],[0.714286,-0.714286],[0.666667,-0.666667],[0.571429,-0.619048],[0.380952,-0.619048],[0.285714,-0.571429],[0.238095,-0.52381],[0.190476,-0.428571],[0.190476,-0.238095],[0.238095,-0.142857],[0.285714,-0.095238],[0.380952,-0.047619],[0.571429,-0.047619],[0.666667,-0.095238],[0.714286,-0.142857],[0.761905,-0.238095],[0.761905,-0.428571],[0.714286,-0.52381],[0.666667,-0.571429],[0.571429,-0.619048]]],"w":0.952381},"9":{"l":[[[0.285714,-0.047619],[0.47619,-0.047619],[0.571429,-0.095238],[0.619048,-0.142857],[0.714286,-0.285714],[0.761905,-0.47619],[0.761905,-0.857143],[0.714286,-0.952381],[0.666667,-1],[0.571429,-1.047619],[0.380952,-1.047619],[0.285714,-1],[0.238095,-0.952381],[0.190476,-0.857143],[0.190476,-0.619048],[0.238095,-0.52381],[0.285714,-0.47619],[0.380952,-0.428571],[0.571429,-0.428571],[0.666667,-0.47619],[0.714286,-0.52381],[0.761905,-0.619048]]],"w":0.952381},"V":{"l":[[[0.095238,-1.047619],[0.428571,-0.047619],[0.761905,-1.047619]]],"w":0.857143},"-":{"l":[[[0.238095,-0.428571],[1,-0.428571]]],"w":1.238095},",":{"l":[[[0.285714,-0.095238],[0.285714,-0.047619],[0.238095,0.047619],[0.190476,0.095238]]],"w":0.47619},"/":{"l":[[[0.952381,-1.095238],[0.095238,0.190476]]],"w":1.047619},".":{"l":[[[0.238095,-0.142857],[0.285714,-0.095238],[0.238095,-0.047619],[0.190476,-0.095238],[0.238095,-0.142857],[0.238095,-0.047619]]],"w":0.47619},"µ":{"l":[[[0.238095,-0.714286],[0.238095,0.285714]],[[0.714286,-0.190476],[0.761905,-0.095238],[0.857143,-0.047619]],[[0.238095,-0.190476],[0.285714,-0.095238],[0.380952,-0.047619],[0.571429,-0.047619],[0.666667,-0.095238],[0.714286,-0.190476],[0.714286,-0.714286]]],"w":1.047619},"A":{"l":[[[0.190476,-0.333333],[0.666667,-0.333333]],[[0.095238,-0.047619],[0.428571,-1.047619],[0.761905,-0.047619]]],"w":0.857143},"C":{"l":[[[0.809524,-0.142857],[0.761905,-0.095238],[0.619048,-0.047619],[0.52381,-0.047619],[0.380952,-0.095238],[0.285714,-0.190476],[0.238095,-0.285714],[0.190476,-0.47619],[0.190476,-0.619048],[0.238095,-0.809524],[0.285714,-0.904762],[0.380952,-1],[0.52381,-1.047619],[0.619048,-1.047619],[0.761905,-1],[0.809524,-0.952381]]],"w":1},"B":{"l":[[[0.571429,-0.571429],[0.714286,-0.52381],[0.761905,-0.47619],[0.809524,-0.380952],[0.809524,-0.238095],[0.761905,-0.142857],[0.714286,-0.095238],[0.619048,-0.047619],[0.238095,-0.047619],[0.238095,-1.047619],[0.571429,-1.047619],[0.666667,-1],[0.714286,-0.952381],[0.761905,-0.857143],[0.761905,-0.761905],[0.714286,-0.666667],[0.666667,-0.619048],[0.571429,-0.571429],[0.238095,-0.571429]]],"w":1},"E":{"l":[[[0.238095,-0.571429],[0.571429,-0.571429]],[[0.714286,-0.047619],[0.238095,-0.047619],[0.238095,-1.047619],[0.714286,-1.047619]]],"w":0.904762},"D":{"l":[[[0.238095,-0.047619],[0.238095,-1.047619],[0.47619,-1.047619],[0.619048,-1],[0.714286,-0.904762],[0.761905,-0.809524],[0.809524,-0.619048],[0.809524,-0.47619],[0.761905,-0.285714],[0.714286,-0.190476],[0.619048,-0.095238],[0.47619,-0.047619],[0.238095,-0.047619]]],"w":1},"G":{"l":[[[0.761905,-1],[0.666667,-1.047619],[0.52381,-1.047619],[0.380952,-1],[0.285714,-0.904762],[0.238095,-0.809524],[0.190476,-0.619048],[0.190476,-0.47619],[0.238095,-0.285714],[0.285714,-0.190476],[0.380952,-0.095238],[0.52381,-0.047619],[0.619048,-0.047619],[0.761905,-0.095238],[0.809524,-0.142857],[0.809524,-0.47619],[0.619048,-0.47619]]],"w":1},"F":{"l":[[[0.571429,-0.571429],[0.238095,-0.571429]],[[0.238095,-0.047619],[0.238095,-1.047619],[0.714286,-1.047619]]],"w":0.857143},"I":{"l":[[[0.238095,-0.047619],[0.238095,-1.047619]]],"w":0.47619},"H":{"l":[[[0.238095,-0.047619],[0.238095,-1.047619]],[[0.238095,-0.571429],[0.809524,-0.571429]],[[0.809524,-0.047619],[0.809524,-1.047619]]],"w":1.047619},"K":{"l":[[[0.238095,-0.047619],[0.238095,-1.047619]],[[0.809524,-0.047619],[0.380952,-0.619048]],[[0.809524,-1.047619],[0.238095,-0.47619]]],"w":1},"J":{"l":[[[0.52381,-1.047619],[0.52381,-0.333333],[0.47619,-0.190476],[0.380952,-0.095238],[0.238095,-0.047619],[0.142857,-0.047619]]],"w":0.761905},"M":{"l":[[[0.238095,-0.047619],[0.238095,-1.047619],[0.571429,-0.333333],[0.904762,-1.047619],[0.904762,-0.047619]]],"w":1.142857},"L":{"l":[[[0.714286,-0.047619],[0.238095,-0.047619],[0.238095,-1.047619]]],"w":0.809524},"O":{"l":[[[0.428571,-1.047619],[0.619048,-1.047619],[0.714286,-1],[0.809524,-0.904762],[0.857143,-0.714286],[0.857143,-0.380952],[0.809524,-0.190476],[0.714286,-0.095238],[0.619048,-0.047619],[0.428571,-0.047619],[0.333333,-0.095238],[0.238095,-0.190476],[0.190476,-0.380952],[0.190476,-0.714286],[0.238095,-0.904762],[0.333333,-1],[0.428571,-1.047619]]],"w":1.047619},"N":{"l":[[[0.238095,-0.047619],[0.238095,-1.047619],[0.809524,-0.047619],[0.809524,-1.047619]]],"w":1.047619},"Q":{"l":[[[0.904762,0.047619],[0.809524,0],[0.714286,-0.095238],[0.571429,-0.238095],[0.47619,-0.285714],[0.380952,-0.285714]],[[0.428571,-0.047619],[0.333333,-0.095238],[0.238095,-0.190476],[0.190476,-0.380952],[0.190476,-0.714286],[0.238095,-0.904762],[0.333333,-1],[0.428571,-1.047619],[0.619048,-1.047619],[0.714286,-1],[0.809524,-0.904762],[0.857143,-0.714286],[0.857143,-0.380952],[0.809524,-0.190476],[0.714286,-0.095238],[0.619048,-0.047619],[0.428571,-0.047619]]],"w":1.047619},"P":{"l":[[[0.238095,-0.047619],[0.238095,-1.047619],[0.619048,-1.047619],[0.714286,-1],[0.761905,-0.952381],[0.809524,-0.857143],[0.809524,-0.714286],[0.761905,-0.619048],[0.714286,-0.571429],[0.619048,-0.52381],[0.238095,-0.52381]]],"w":1},"S":{"l":[[[0.190476,-0.095238],[0.333333,-0.047619],[0.571429,-0.047619],[0.666667,-0.095238],[0.714286,-0.142857],[0.761905,-0.238095],[0.761905,-0.333333],[0.714286,-0.428571],[0.666667,-0.47619],[0.571429,-0.52381],[0.380952,-0.571429],[0.285714,-0.619048],[0.238095,-0.666667],[0.190476,-0.761905],[0.190476,-0.857143],[0.238095,-0.952381],[0.285714,-1],[0.380952,-1.047619],[0.619048,-1.047619],[0.761905,-1]]],"w":0.952381},"R":{"l":[[[0.809524,-0.047619],[0.47619,-0.52381]],[[0.238095,-0.047619],[0.238095,-1.047619],[0.619048,-1.047619],[0.714286,-1],[0.761905,-0.952381],[0.809524,-0.857143],[0.809524,-0.714286],[0.761905,-0.619048],[0.714286,-0.571429],[0.619048,-0.52381],[0.238095,-0.52381]]],"w":1},"U":{"l":[[[0.238095,-1.047619],[0.238095,-0.238095],[0.285714,-0.142857],[0.333333,-0.095238],[0.428571,-0.047619],[0.619048,-0.047619],[0.714286,-0.095238],[0.761905,-0.142857],[0.809524,-0.238095],[0.809524,-1.047619]]],"w":1.047619},"T":{"l":[[[0.095238,-1.047619],[0.666667,-1.047619]],[[0.380952,-0.047619],[0.380952,-1.047619]]],"w":0.761905},"W":{"l":[[[0.142857,-1.047619],[0.380952,-0.047619],[0.571429,-0.761905],[0.761905,-0.047619],[1,-1.047619]]],"w":1.142857},"Y":{"l":[[[0.428571,-0.52381],[0.428571,-0.047619]],[[0.095238,-1.047619],[0.428571,-0.52381],[0.761905,-1.047619]]],"w":0.857143},"_":{"l":[[[0,0.047619],[0.761905,0.047619]]],"w":0.761905},"n":{"l":[[[0.238095,-0.714286],[0.238095,-0.047619]],[[0.238095,-0.619048],[0.285714,-0.666667],[0.380952,-0.714286],[0.52381,-0.714286],[0.619048,-0.666667],[0.666667,-0.571429],[0.666667,-0.047619]]],"w":0.904762},"u":{"l":[[[0.666667,-0.714286],[0.666667,-0.047619]],[[0.238095,-0.714286],[0.238095,-0.190476],[0.285714,-0.095238],[0.380952,-0.047619],[0.52381,-0.047619],[0.619048,-0.095238],[0.666667,-0.142857]]],"w":0.904762},"x":{"l":[[[0.142857,-0.047619],[0.666667,-0.714286]],[[0.142857,-0.714286],[0.666667,-0.047619]]],"w":0.809524}};
    // @formatter:on
}

function fromaltium(_data_) {
    var result = {};
    var bom = {};
    bom.B = [];
    bom.F = [];
    bom.both = [];
    bom.fields = {};
    bom.skipped = [];

    var footprints = [];

    var drawings = {};
    drawings.fabrication = {};
    drawings.fabrication.B = [];
    drawings.fabrication.F = [];

    drawings.silkscreen = {};
    drawings.silkscreen.B = [];
    drawings.silkscreen.F = [];

    var tracks = {};
    tracks.B = [];
    tracks.F = [];

    var zones = {};
    zones.B = [];
    zones.F = [];

    var edges = [];

    var edges_bbox = {};

    var metadata = {};

    var index = 0;

    var _B = {};
    var _F = {};
    var _both = {};

    var nets = [];
    var _nets = {};
    nets.push("No Net");
    _nets["No Net"] = "No Net";

    var font_data = getfont();

    for (const key in _data_.Data) {
        var item = _data_.Data[key];

        //TODO: NoBOM<>Skipped looks like it DNP
        //bom.skipped.push(index);
        if (!item.NoBOM) {
            var group = JSON.stringify([item.PartNumber, item.Footprint]);
            if (item.Layer == "TopLayer") {
                if (!(group in _F)) {
                    _F[group] = [];
                }
                _F[group].push([item.Designator, index]);
            }
            if (item.Layer == "BottomLayer") {
                if (!(group in _B)) {
                    _B[group] = [];
                }
                _B[group].push([item.Designator, index]);
            }
            if (!(group in _both)) {
                _both[group] = [];
            }
            _both[group].push([item.Designator, index]);
        }

        bom.fields[index] = [item.PartNumber, item.Footprint];

        var footprint = {};
        footprint.bbox = {};
        footprint.bbox.angle = 0;
        footprint.bbox.pos = [item.X * 1, item.Y * 1];
        footprint.bbox.relpos = [0, 0];//[-Math.round(item.Width / 2), -Math.round(item.Height / 2)];
        //footprint.bbox.size = [Math.round(item.Width), Math.round(item.Height)];
        footprint.bbox.size = [item.Width * 1, item.Height * 1];

        footprint.drawings = [];
        if (item.Layer == "TopLayer") footprint.layer = "F";
        if (item.Layer == "BottomLayer") footprint.layer = "B";

        footprint.pads = [];

        for (const key2 in item.Pads) {
            var itemPad = item.Pads[key2];

            var pad = {};
            pad.angle = itemPad.Angle * 1;
            if (itemPad.Type == "th") {
                pad.drillshape = itemPad.DrillShape;
                pad.drillsize = [itemPad.DrillWidth * 1, itemPad.DrillHeight * 1];
            }
            if (itemPad.Layer == "TopLayer") pad.layers = ["F"];
            if (itemPad.Layer == "BottomLayer") pad.layers = ["B"];
            if (itemPad.Layer == "MultiLayer") pad.layers = ["F", "B"];
            pad.offset = [0, 0];
            //pad.path2d = {};
            if (itemPad.Pin1 == "1") {
                pad.pin1 = itemPad.Pin1;
            }
            pad.pos = [itemPad.X * 1, itemPad.Y * 1];
            pad.shape = itemPad.Shape;
            pad.size = [itemPad.Width * 1, itemPad.Height * 1];
            pad.type = itemPad.Type;
            if (_data_.Settings.AddNets) {
                //TODO: always true
                pad.net = itemPad.Net;
                if (itemPad.Net != "") {
                    if (!(itemPad.Net in _nets)) {
                        _nets[itemPad.Net] = itemPad.Net;
                        nets.push(itemPad.Net);
                    }
                }
            }

            footprint.pads.push(pad);
        }

        footprint.ref = item.Designator;
        footprints.push(footprint);

        index++;
    }

    for (const key in _F) {
        var _group = _F[key];
        bom.F.push(_group);
    }

    for (const key in _B) {
        var _group = _B[key];
        bom.B.push(_group);
    }

    for (const key in _both) {
        var _group = _both[key];
        bom.both.push(_group);
    }

    for (const key in _data_.Board) {
        var item = _data_.Board[key];

        if (item.Type == "segment") {
            var segment = {};
            segment.end = [item.X2 * 1, item.Y2 * 1];
            segment.start = [item.X1 * 1, item.Y1 * 1];
            segment.type = "segment";
            segment.width = item.Width;
        }

        if (item.Type == "arc") {
            var segment = {};
            segment.start = [item.X * 1, item.Y * 1];
            segment.endangle = item.Angle2 * 1;
            segment.width = item.Width * 1;
            segment.radius = item.Radius * 1;
            segment.startangle = item.Angle1 * 1;
            segment.type = "arc";
        }

        edges.push(segment);
    }

    for (const key in _data_.Extra) {
        var item = _data_.Extra[key];

        if (item.Type == "segment") {
            var segment = {};
            segment.end = [item.X2 * 1, item.Y2 * 1];
            segment.start = [item.X1 * 1, item.Y1 * 1];
            segment.type = "segment";
            segment.width = item.Width * 1;

            if (_data_.Settings.AddNets) {
                if ((item.Layer == "TopLayer") || (item.Layer == "BottomLayer")) {
                    segment.net = item.Net;
                    //TODO: always true
                    if (item.Net != "") {
                        if (!(item.Net in _nets)) {
                            _nets[item.Net] = item.Net;
                            nets.push(item.Net);
                        }
                    }
                }
            }

            if (item.Layer == "TopOverlay") drawings.silkscreen.F.push(segment);
            if (item.Layer == "BottomOverlay") drawings.silkscreen.B.push(segment);
            if (item.Layer == "TopLayer") tracks.F.push(segment);
            if (item.Layer == "BottomLayer") tracks.B.push(segment);
        }
        if (item.Type == "via") {
            var segment = {};
            segment.end = [item.X * 1, item.Y * 1];
            segment.start = [item.X * 1, item.Y * 1];
            segment.type = "segment";
            segment.width = item.Width * 1;

            if (_data_.Settings.AddNets) {
                if ((item.Layer == "TopLayer") || (item.Layer == "BottomLayer") || (item.Layer == "MultiLayer")) {
                    segment.net = item.Net;
                    //TODO: always true
                    if (item.Net != "") {
                        if (!(item.Net in _nets)) {
                            _nets[item.Net] = item.Net;
                            nets.push(item.Net);
                        }
                    }
                }
            }

            if (item.Layer == "TopLayer") tracks.F.push(segment);
            if (item.Layer == "BottomLayer") tracks.B.push(segment);
            if (item.Layer == "MultiLayer") {
                tracks.F.push(segment);
                tracks.B.push(segment);
            }

            if (_data_.Settings.AddViasHoles) {
                var footprint = {};
                footprint.bbox = {};
                footprint.bbox.angle = 0;
                footprint.bbox.pos = [0, 0];
                footprint.bbox.relpos = [0, 0];
                footprint.bbox.size = [0, 0];

                footprint.drawings = [];
                //TODO: no matter
                footprint.layer = "F";

                footprint.pads = [];

                var pad = {};
                pad.angle = 0;
                pad.drillshape = "circle";
                pad.drillsize = [item.DrillWidth * 1, item.DrillWidth * 1];
                if (item.Layer == "TopLayer") pad.layers = ["F"];
                if (item.Layer == "BottomLayer") pad.layers = ["B"];
                if (item.Layer == "MultiLayer") pad.layers = ["F", "B"];
                pad.offset = [0, 0];
                pad.pos = [item.X * 1, item.Y * 1];
                pad.shape = "circle";
                pad.size = [item.Width * 1, item.Width * 1];
                pad.type = "th";
                if (_data_.Settings.AddNets) {
                    //TODO: always true
                    pad.net = itemPad.Net;
                    if (itemPad.Net != "") {
                        if (!(itemPad.Net in _nets)) {
                            _nets[itemPad.Net] = itemPad.Net;
                            nets.push(itemPad.Net);
                        }
                    }
                }

                footprint.pads.push(pad);
                footprints.push(footprint);
            }
        }
        if (item.Type == "arc") {
            var segment = {};
            segment.start = [item.X * 1, item.Y * 1];
            segment.endangle = item.Angle2 * 1;
            segment.width = item.Width * 1;
            segment.radius = item.Radius * 1;
            segment.startangle = item.Angle1 * 1;
            segment.type = "arc";

            if (item.Layer == "TopOverlay") drawings.silkscreen.F.push(segment);
            if (item.Layer == "BottomOverlay") drawings.silkscreen.B.push(segment);
        }
        if (item.Type == "text") {
            var segment = {};
            segment.angle = item.Angle * 1;
            segment.attr = [];
            if (item.Mirrored) {
                segment.attr.push("mirrored");
            }
            segment.height = item.Height * 1;
            segment.justify = [0, 0];
            segment.pos = [item.X * 1, item.Y * 1];
            if (item.Designator) {
                segment.ref = 1;
            }
            if (item.Value) {
                segment.val = 1;
            }
            segment.text = fixtext(item.Text, font_data);
            segment.thickness = 0.15;
            segment.width = item.Width * 1;

            if (item.Layer == "TopOverlay") drawings.silkscreen.F.push(segment);
            if (item.Layer == "BottomOverlay") drawings.silkscreen.B.push(segment);
        }
        if (item.Type == "polygon") {
            var segment = {};
            //segment.end = [item.X2 * 1, item.Y2 * 1];
            //segment.start = [item.X1 * 1, item.Y1 * 1];
            //segment.type = "polygon";
            //segment.width = 1;//item.Width * 1;


            if ((item.Layer == "TopOverlay") || (item.Layer == "BottomOverlay")) {
                //segment.end = [item.X2 * 1, item.Y2 * 1];
                //segment.start = [item.X1 * 1, item.Y1 * 1];
                segment.type = "polygon";
                //"filled": 1,
                //segment.width = 1;//item.Width * 1;

                segment.angle = 0;
                //segment.net: "No Net"
                segment.pos = [0, 0];
            }


            if (_data_.Settings.AddNets) {
                if ((item.Layer == "TopLayer") || (item.Layer == "BottomLayer")) {
                    segment.net = item.Net;
                    //TODO: always true
                    if (item.Net != "") {
                        if (!(item.Net in _nets)) {
                            _nets[item.Net] = item.Net;
                            nets.push(item.Net);
                        }
                    }
                }
            }

            var polygons = [];

            /*
            for (const key in item.Points) {
                var _p_ = item.Points[key];
                polygons.push(_p_);
            }
             */
            for (const _key_ in item.EX) {
                var _ex_ = item.EX[_key_];
                var p = [];
                for (const _key2_ in _ex_) {
                    p.push(_ex_[_key2_]);
                }
                polygons.push(p);
            }

            segment.polygons = polygons;//[polygons];

            if (item.Layer == "TopOverlay") drawings.silkscreen.F.push(segment);
            if (item.Layer == "BottomOverlay") drawings.silkscreen.B.push(segment);
            if (item.Layer == "TopLayer") zones.F.push(segment);
            if (item.Layer == "BottomLayer") zones.B.push(segment);
        }
    }

    edges_bbox.maxx = _data_.BB.X2;
    edges_bbox.maxy = _data_.BB.Y2;
    edges_bbox.minx = _data_.BB.X1;
    edges_bbox.miny = _data_.BB.Y1;

    metadata.company = _data_.Metadata.Company;
    metadata.date = _data_.Metadata.Date;
    metadata.revision = _data_.Metadata.Revision;
    metadata.title = _data_.Metadata.Title;

    result.bom = bom;
    result.footprints = footprints;
    result.drawings = drawings;
    result.edges = edges;
    result.edges_bbox = edges_bbox;
    result.metadata = metadata;
    if (_data_.Settings.AddNets) {
        result.nets = nets;
    }
    if (_data_.Settings.AddTracks) {
        result.tracks = tracks;
        result.zones = zones;
    }

    result.ibom_version = "v2.9.0";

    result.font_data = font_data;

    return result;
}

var pcbdata = fromaltium(altiumbom);