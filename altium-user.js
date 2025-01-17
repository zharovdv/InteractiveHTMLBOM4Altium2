//NOTE: render.js drawPadHole hack for rectangle pad drill hole support
drawPadHole = function (ctx, pad, padHoleColor) {
    if (pad.type != "th") return;
    ctx.save();
    ctx.translate(...pad.pos);
    ctx.rotate(-deg2rad(pad.angle));
    ctx.fillStyle = padHoleColor;
    if (pad.drillshape == "oblong") {
        ctx.fill(getOblongPath(pad.drillsize));
    } else if (pad.drillshape == "rect") {
        ctx.fill(getChamferedRectPath(pad.drillsize, 0, 0, 0));
    } else {
        ctx.fill(getCirclePath(pad.drillsize[0] / 2));
    }
    ctx.restore();
};