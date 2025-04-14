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

function fromaltium(_data_) {
    /*

    var tracks = {};
    tracks.B = [];
    tracks.F = [];

    var zones = {};
    zones.B = [];
    zones.F = [];

    var nets = [];
    var _nets = {};
    nets.push("No Net");
    _nets["No Net"] = "No Net";

     */
    /*
        for (const key in _data_.Data) {
            var item = _data_.Data[key];

            //TODO: NoBOM<>Skipped looks like it DNP
            //bom.skipped.push(index);
            if (!item.NoBOM) {



        if (_data_.Settings.AddNets) {
            result.nets = nets;
        }
        if (_data_.Settings.AddTracks) {
            result.tracks = tracks;
            result.zones = zones;
        }
    */

    var bom = {};
    bom.B = [];
    bom.F = [];
    bom.both = [];
    bom.fields = {};
    bom.skipped = [];

    var index = 0;

    var _B = {};
    var _F = {};
    var _both = {};

    for (const key in _data_.components) {
        var item = _data_.components[key];
        var groupKey = JSON.stringify([item.footprint]);

        if (item.layer == "F") {
            if (!(groupKey in _F)) {
                _F[groupKey] = [];
            }
            _F[groupKey].push([item.ref, index]);
        }

        if (item.layer == "B") {
            if (!(groupKey in _B)) {
                _B[groupKey] = [];
            }
            _B[groupKey].push([item.ref, index]);
        }

        if (!(groupKey in _both)) {
            _both[groupKey] = [];
        }
        _both[groupKey].push([item.ref, index]);

        bom.fields[index] = [item.footprint, "F"];

        index++;
    }

    for (const key in _F) {
        var group = _F[key];
        bom.F.push(group);
    }

    for (const key in _B) {
        var group = _B[key];
        bom.B.push(group);
    }

    for (const key in _both) {
        var group = _both[key];
        bom.both.push(group);
    }

    var result = {};

    result.bom = bom;

    result.drawings = _data_.pcbdata.drawings;
    result.edges = _data_.pcbdata.edges;
    result.edges_bbox = _data_.pcbdata.edges_bbox;
    result.font_data = _data_.pcbdata.font_data;
    result.footprints = _data_.pcbdata.footprints;
    result.metadata = _data_.pcbdata.metadata;
    result.nets = _data_.pcbdata.nets;
    result.tracks = _data_.pcbdata.tracks;
    result.zones = _data_.pcbdata.zones;

    result.ibom_version = "v2.10.0";

    return result;
}

var pcbdata = fromaltium(altiumbom);