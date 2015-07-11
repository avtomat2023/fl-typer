jQuery(function($) {
    var marginOfSubtrees = 10;
    var edgeHeight = 20;
    var svgMargin = 10;
    var svgNS = "http://www.w3.org/2000/svg";
    var textVerticalMargin = 5;
    var textSize = 30;
    var subscriptSize = 15;
    var subscriptYOffset = 5;

    // kindが"richtext"であるJSONを描画した時のサイズを計算する
    // JSON自体ではなくcontentsを受け取る
    function getRichtextSize(contents) {
        // ダミーのsvg要素に描画（g要素を追加）する
        var g = richtextToSvgG(contents, 'left', 0, 0);
        var svg = $('#dummy-svg').get(0);
        svg.appendChild(g);
        // ダミーのsvg要素を表示
        svg.setAttribute('display', 'inline');
        // Bounding Boxを取得
        var bbox = g.getBBox();
        var size = {
            width: bbox.width,
            height: bbox.height
        };
        // ダミーのsvg要素を隠す
        // Javascript実行中に表示・非表示を切り替えるので、
        // 実際にブラウザ上に表示されることはないはず
        svg.setAttribute('display', 'none');
        // 追加したg要素を削除
        svg.removeChild(g);
        return size;
    }

    // kindが"richtext"であるJSONをSVGのg要素に変換する
    // JSON自体ではなくcontentsを受け取る
    // anchorは左右のアラインメントで、"left", "middle", "right"で指定
    // x, yは表示位置 xはアラインメントに従う yはテキスト上端の位置
    function richtextToSvgG(contents, anchor, x, y) {
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
            // y座標を上端だと解釈するよう、基底線を指定
            tspan.setAttributeNS(null, 'dominant-baseline', 'text-before-edge');
            // 下付き文字の処理
            var subscripted = false;
            if (contents[i].subscript) {
                subscripted = true;
                tspan.setAttributeNS(null, 'font-size', subscriptSize.toString());
                dy += subscriptYOffset;
            }
            if (dy != 0)
                tspan.setAttributeNS(null, 'dy', dy.toString());
            if (subscripted)
                dy = -subscriptYOffset;
            else
                dy = 0;
            // 斜体の処理
            if (contents[i].italic)
                tspan.setAttributeNS(null, 'font-style', 'italic');

            var textNode = document.createTextNode(contents[i].text);
            tspan.appendChild(textNode);
            text.appendChild(tspan);
        }

        g.appendChild(text);
        return g;
    }

    // (x1, y1)から(x2, y2)への直線を表す、SVGのg要素を作る
    function createLineSvgG(x1, y1, x2, y2) {
        var g = document.createElementNS(svgNS, 'g');
        g.setAttributeNS(null, 'stroke', 'black');
        g.setAttributeNS(null, 'stroke-width', '2');
        g.setAttributeNS(null, 'stroke-linecap', 'round');

        var line = document.createElementNS(svgNS, 'line');
        line.setAttributeNS(null, 'x1', x1.toString());
        line.setAttributeNS(null, 'y1', y1.toString());
        line.setAttributeNS(null, 'x2', x2.toString());
        line.setAttributeNS(null, 'y2', y2.toString());

        g.appendChild(line);
        return g;
    }

    // svg要素のhtml表現を作る
    // width, heightはsvgの内容物のサイズを指定する
    // 実際のsvg要素のサイズは、svgMarginの二倍(両端)を加算したものとなる
    function createSvgHtml(id, width, height) {
        return '<svg id="' + id + '" xmls="' + svgNS + '" version="1.1" ' +
            'width="' + (width + svgMargin*2).toString() + '" ' +
            'height="' + (height + svgMargin*2).toString() + '">' +
            '</svg>';
    }

    // サーバから受け取ったASTに、サイズ情報を付加する
    function attachSize(ast) {
        if (ast.kind == "richtext") {
            var size = getRichtextSize(ast.contents);
            ast.width = size.width;
            ast.height = size.height + textVerticalMargin*2;
        } else { // kind == "tree"
            attachSize(ast.node)
            for (var i in ast.children)
                attachSize(ast.children[i])

            var widthOfChildren = 0
            for (var i in ast.children)
                widthOfChildren += ast.children[i].width;
            widthOfChildren += marginOfSubtrees * (ast.children.length-1);

            var heightOfChildren = 0;
            for (var i in ast.children)
                if (heightOfChildren < ast.children[i].height)
                    heightOfChildren = ast.children[i].height;

            var x = -widthOfChildren / 2;
            for (var i in ast.children) {
                ast.children[i] = {
                    relativeX: x, // 左端
                    tree: ast.children[i]
                }
                x += ast.children[i].tree.width + marginOfSubtrees;
            }

            ast.width = Math.max(ast.node.width, widthOfChildren);
            ast.height = ast.node.height + edgeHeight + heightOfChildren;
        }
    }

    // サーバから受け取った式を描画
    function drawExpr(expr) {
        var size = getRichtextSize(expr.contents);
        var svg = createSvgHtml('expr-svg', size.width, size.height);
        $('#expr-panel').html(svg);
        var g = richtextToSvgG(expr.contents, 'left', svgMargin, svgMargin);
        $('#expr-svg').get(0).appendChild(g);
    }

    // サイズ情報の付いたASTをsvg要素に描画
    // xは中央、yは上端
    function drawSizedAst(svg, ast, x, y) {
        if (ast.kind == "richtext") {
            svg.appendChild(richtextToSvgG(ast.contents, 'middle',
                                           x, y + textVerticalMargin));
        } else { // kind == "tree"
            drawSizedAst(svg, ast.node, x, y);
            var botOfHead = y + ast.node.height;
            var nextY = botOfHead + edgeHeight;
            for (var i in ast.children) {
                var node = ast.children[i];
                var nextX = x + node.relativeX + node.tree.width/2;
                drawSizedAst(svg, node.tree, nextX, nextY);

                svg.appendChild(createLineSvgG(x, botOfHead, nextX, nextY));
            }
        }
    }

    // サーバから受け取ったASTを描画
    // astは破壊的に変更されるため、再利用できない
    function drawAst(ast) {
        attachSize(ast);
        // 描画先であるsvg要素を作成
        var svg = createSvgHtml("ast-svg", ast.width, ast.height);
        $('#ast-panel').html(svg);
        // 描画
        var originX = svgMargin + ast.width/2;
        var originY = svgMargin;
        drawSizedAst($('#ast-svg').get(0), ast, originX, originY);
    }

    $('#success-panel-group').hide()
    $('#error-panel-group').hide()

    // ダミーのsvg要素を、読み込みが完了した瞬間に非表示にする
    $('#dummy-svg').ready(function() {
        $('#dummy-svg').get(0).setAttribute('display', 'none');
    });

    // 式を入力するテキストボックスで
    // バックスラッシュと円マークをラムダに変換する処理
    // および、Enterキーが押されたら型付けボタンを押す処理
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

    // サンプルプログラムのセレクタが選択された時の処理
    var samplePrograms = {
        'fst': 'λx.y.x',
        'snd': 'λx.y.y',
        'List Construction': '1::2::3::4::nil'
    }
    $('#sample-select').on('change', function() {
        var key = $(this).find("option:selected").val();
        if (key in samplePrograms)
            $('#expression').val(samplePrograms[key]);
    });

    // 型付けボタンが押された時のAjax通信処理
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
                    drawExpr(json.expr);
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
