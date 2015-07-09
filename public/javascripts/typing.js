jQuery(function($) {
    var marginOfSubtrees = 10;
    var edgeHeight = 20;
    var svgMargin = 10;
    var svgNS = "http://www.w3.org/2000/svg";
    var textVerticalMargin = 5;
    var textSize = 30;
    var subscriptSize = 15;
    var subscriptYOffset = 5;

    function getRichtextSize(contents) {
        var g = richtextToSvg(contents, 'left', 0, 0);
        var svg = $('#dummy-svg').get(0);
        svg.appendChild(g);
        svg.setAttribute('display', 'inline');
        var bbox = g.getBBox();
        var size = {
            width: bbox.width,
            height: bbox.height
        };
        svg.setAttribute('display', 'none');
        svg.removeChild(g);
        return size;
    }

    function richtextToSvg(contents, anchor, x, y) {
        var g = document.createElementNS(svgNS, 'g');
        g.setAttributeNS(null, 'font-family', 'XITS');
        g.setAttributeNS(null, 'font-size', textSize.toString());
        g.setAttributeNS(null, 'fill', 'black');

        var text = document.createElementNS(svgNS, 'text');
        text.setAttributeNS(null, 'text-anchor', anchor);
        text.setAttributeNS(null, 'x', x.toString());
        text.setAttributeNS(null, 'y', y.toString());

        var dy = 0;
        for (var i in contents) {
            var tspan = document.createElementNS(svgNS, 'tspan');
            tspan.setAttributeNS(null, 'dominant-baseline', 'text-before-edge');
            if (contents[i].subscript) {
                tspan.setAttributeNS(null, 'font-size', subscriptSize.toString());
                dy += subscriptYOffset;
            }
            if (dy != 0)
                tspan.setAttributeNS(null, 'dy', dy.toString());
            if (contents[i].italic)
                tspan.setAttributeNS(null, 'font-style', 'italic');

            var textNode = document.createTextNode(contents[i].text);
            tspan.appendChild(textNode);
            text.appendChild(tspan);
        }

        g.appendChild(text);
        return g;
    }

    // サーバから受け取ったdrawable ASTに、サイズ情報を付加する
    function attachSize(drawable) {
        if (drawable.kind == "richtext") {
            // FIXME
            var size = getRichtextSize(drawable.contents);
            drawable.width = size.width;
            drawable.height = size.height + textVerticalMargin*2;
        } else { // kind == "tree"
            attachSize(drawable.node)
            for (var i in drawable.children)
                attachSize(drawable.children[i])

            var widthOfChildren = 0
            for (var i in drawable.children)
                widthOfChildren += drawable.children[i].width;
            widthOfChildren += marginOfSubtrees * (drawable.children.length-1);

            var heightOfChildren = 0;
            for (var i in drawable.children)
                if (heightOfChildren < drawable.children[i].height)
                    heightOfChildren = drawable.children[i].height;

            var x = -widthOfChildren / 2;
            for (var i in drawable.children) {
                drawable.children[i] = {
                    relativeX: x, // 左端
                    tree: drawable.children[i]
                }
                x += drawable.children[i].tree.width + marginOfSubtrees;
            }

            drawable.width = Math.max(drawable.node.width, widthOfChildren);
            drawable.height = drawable.node.height + edgeHeight + heightOfChildren;
        }
    }

    // SVG要素に描画
    // xは中央、yは上端
    function draw(svg, drawable, x, y) {
        var svgNS = "http://www.w3.org/2000/svg";

        if (drawable.kind == "richtext") {
            svg.appendChild(richtextToSvg(drawable.contents, 'middle',
                                          x, y + textVerticalMargin));
        } else { // kind == "tree"
            draw(svg, drawable.node, x, y);
            var botOfHead = y + drawable.node.height;
            var nextY = botOfHead + edgeHeight;
            for (var i in drawable.children) {
                var node = drawable.children[i];
                var nextX = x + node.relativeX + node.tree.width/2;
                draw(svg, node.tree, nextX, nextY);

                var g = document.createElementNS(svgNS, 'g');
                g.setAttributeNS(null, 'stroke', 'black');
                g.setAttributeNS(null, 'stroke-width', '2');
                g.setAttributeNS(null, 'stroke-linecap', 'round');
                var line = document.createElementNS(svgNS, 'line');
                line.setAttributeNS(null, 'x1', x.toString());
                line.setAttributeNS(null, 'y1', botOfHead.toString());
                line.setAttributeNS(null, 'x2', nextX.toString());
                line.setAttributeNS(null, 'y2', nextY.toString());
                g.appendChild(line);
                svg.appendChild(g);
            }
        }
    }

    var drawAst = function(drawable) {
        attachSize(drawable);

        // SVGをセット
        var svg = '<svg id="ast-svg" xmls="' + svgNS + '" version="1.1" ' +
            'width="' + (drawable.width + svgMargin*2).toString() + '" ' +
            'height="' + (drawable.height + svgMargin*2).toString() + '">' +
            '</svg>';
        $('#ast-panel').html(svg);
        var originX = svgMargin + drawable.width/2;
        var originY = svgMargin;
        draw($('#ast-svg').get(0), drawable, originX, originY);
    }

    $('#success-panel-group').hide()
    $('#error-panel-group').hide()

    $('body').append('<svg id="dummy-svg" xmlns="' + svgNS + '" version="1.1" display="none">');

    $('#expression').keypress(function(event) {
        var enter = 13;
        var backslash = '\\'.charCodeAt(0);
        var yen = '¥'.charCodeAt(0);

        var code = event.keyCode;
        if (code == enter) {
            $('#type-button').click();
        } else if (code == backslash || code == yen) {
            event.preventDefault();
            var caret = $(this).caret();
            var s = $(this).val();
            $(this).val(s.slice(0, caret) + "λ" + s.slice(caret));
            $(this).caret(caret+1);
        }
    });

    var samplePrograms = {
        'fst': 'λx.y.x',
        'snd': 'λx.y.y',
        'List Constraction': '1::2::3::4::nil'
    }
    $('.sample-menu').click(function() {
        $('#expression').val(samplePrograms[$(this).html()]);
    });

    // http://ginpen.com/2013/05/07/jquery-ajax-form/
    $('#type-button').click(function(event) {
        $.ajax({
            url: '/typing',
            data: { expression: $('#expression').val() },
            timeout: 10000,
            dataType: 'json',

            // 送信前
            beforeSend: function(xhr, settings) {
                // ボタンを無効化し、二重送信を防止
                $(this).attr('disabled', true);
            },
            // 応答後
            complete: function(xhr, textStatus) {
                // ボタンを有効化し、再送信を許可
                $(this).attr('disabled', false);
            },

            // 通信成功時の処理
            success: function(json, textStatus, xhr) {
                if (json.parsed) {
                    drawAst(json.ast);
                    $('#success-panel-group').show()
                    $('#error-panel-group').hide();
                } else {
                    $('#error-message').text(json.error);
                    $('#success-panel-group').hide()
                    $('#error-panel-group').show();
                }
            },

            // 通信失敗時の処理
            error: function(xhr, textStatus, error) {
                alert(textStatus);
            }
        });
    });
});
