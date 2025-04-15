function getfield(_comp_, _fields_) {
    var fields = [];

    for (const key in _fields_) {
        var field = _fields_[key];
        if (field == "Value") {
            fields.push(_comp_.value);
        } else if (field == "Footprint") {
            fields.push(_comp_.footprint);
        } else {
            if (field in _comp_.extra_fields)
                fields.push(_comp_.extra_fields[field]);
            else
                fields.push('');
        }
    }

    return fields;
}

function getgroup(_comp_, _fields_) {
    var fields = [];

    for (const key in _fields_) {
        var field = _fields_[key];
        if (field == "Value") {
            fields.push(_comp_.value);
        } else if (field == "Footprint") {
            fields.push(_comp_.footprint);
        } else {
            if (field in _comp_.extra_fields)
                fields.push(_comp_.extra_fields[field]);
            else
                fields.push('');
        }
    }

    return fields;
}

function fromaltium(_data_, _config_, _altiumconfig_) {
    //TODO: no nets
    // var _nets = {};
    // nets.push("No Net");
    // _nets["No Net"] = "No Net";

    //TODO: NoBOM<>Skipped looks like it DNP
    //bom.skipped.push(index);

    var group_fields = _altiumconfig_.group_fields;
    var show_fields = _config_.fields;

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
        var groupKey = JSON.stringify(getgroup(item, group_fields));

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

        bom.fields[index] = getfield(item, show_fields);

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

var pcbdata = fromaltium(altiumbom, config, altiumconfig);