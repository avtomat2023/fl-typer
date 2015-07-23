jQuery(function($) {
    "use strict";

    var marginOfSubtrees = 10;
    var marginOfExpressions = 30;
    var marginOfDiagramAndEquations = 20;
    // 木構造の枝の高さ
    var edgeHeight = 20;
    var lineWidth = 2;
    var svgMargin = 10;
    var svgNS = "http://www.w3.org/2000/svg";
    // 図中のテキストの上下間隔（上下それぞれ5ずつ）
    var textVerticalMargin = 5;
    var textSize = 30;
    var textYOffset = 23;
    var subscriptSize = 15;
    var subscriptYOffset = 5;
    var smallTextSize = 20;
    var ruleMargin = 5;
    // 型と=記号とのマージン
    var equationMargin = 5;

    // リッチテキストを描画した時のサイズを計算する
    function getRichtextSize(richtext) {
        // ダミーのsvg要素に描画（g要素を追加）する
        var g = richtextToSvgG(richtext, 'start', 0, 0);
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

    // リッチテキストをSVGのg要素に変換する
    // anchorは左右のアラインメントで、"start", "middle", "end"で指定
    // x, yは表示位置 xはアラインメントに従う yはテキスト上端の位置
    function richtextToSvgG(richtext, anchor, x, y) {
        var g = document.createElementNS(svgNS, 'g');
        g.setAttributeNS(null, 'font-family', 'XITS');
        g.setAttributeNS(null, 'font-size', textSize.toString());
        g.setAttributeNS(null, 'fill', 'black');

        var text = document.createElementNS(svgNS, 'text');
        text.setAttributeNS(null, 'text-anchor', anchor);
        text.setAttributeNS(null, 'x', x.toString());
        // 下付き文字については、dominant-baselineを指定するより、
        // y座標を変えたほうがブラウザ間の見え方の差が少ない
        text.setAttributeNS(null, 'y', (y + textYOffset).toString());

        var dy = 0;
        for (var i in richtext) {
            var tspan = document.createElementNS(svgNS, 'tspan');
            // 下付き・垂直中央寄せ文字の処理
            var subscripted = false;
            var middled = false
            if (richtext[i].style == "subscript") {
                subscripted = true;
                tspan.setAttributeNS(null, 'font-size', subscriptSize.toString());
                dy += subscriptYOffset;
            } else if (richtext[i].style == "middle") {
                middled = true;
                tspan.setAttributeNS(null, 'font-size', smallTextSize.toString());
                tspan.setAttributeNS(null, 'dominant-baseline', 'middle');
                dy -= textYOffset;
            }
            if (dy != 0)
                tspan.setAttributeNS(null, 'dy', dy.toString());
            if (subscripted)
                dy = -subscriptYOffset;
            else if (middled)
                dy = textYOffset;
            else
                dy = 0;

            // 斜体の処理
            if (richtext[i].style == "italic")
                tspan.setAttributeNS(null, 'font-style', 'italic');

            var textNode = document.createTextNode(richtext[i].text);
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
        g.setAttributeNS(null, 'stroke-width', lineWidth.toString());
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

    // ASTにサイズ情報(width, height, nodeWidth, nodeHeightフィールド)を付加する
    function attachSizeToAst(ast) {
        var nodeSize = getRichtextSize(ast.node);
        ast.nodeWidth = nodeSize.width;
        ast.nodeHeight = nodeSize.height + textVerticalMargin*2;

        if (ast.children.length == 0) { // leaf
            ast.width = ast.nodeWidth;
            ast.height = ast.nodeHeight;
        } else { // has children
            for (var i in ast.children)
                attachSizeToAst(ast.children[i])

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

            ast.width = Math.max(ast.nodeWidth, widthOfChildren);
            ast.height = ast.nodeHeight + edgeHeight + heightOfChildren;
        }
    }

    // サーバから受け取った式を描画
    function drawExpr(expr) {
        var size = getRichtextSize(expr);
        var svg = createSvgHtml('expr-svg', size.width, size.height);
        $('#expr-panel').html(svg);
        var g = richtextToSvgG(expr, 'start', svgMargin, svgMargin);
        $('#expr-svg').get(0).appendChild(g);
    }

    function drawPrincipalType(type) {
        var size = getRichtextSize(type);
        var svg = createSvgHtml('type-svg', size.width, size.height);
        $('#type-panel').html(svg);
        var g = richtextToSvgG(type, 'start', svgMargin, svgMargin);
        $('#type-svg').get(0).appendChild(g);
    }

    // サイズ情報の付いたASTをsvg要素に描画
    // xは中央、yは上端
    function drawSizedAst(svg, ast, x, y) {
        svg.appendChild(richtextToSvgG(ast.node, 'middle',
                                       x, y + textVerticalMargin));
        var botOfHead = y + ast.nodeHeight;
        var nextY = botOfHead + edgeHeight;
        for (var i in ast.children) {
            var child = ast.children[i];
            var nextX = x + child.relativeX + child.tree.width/2;
            drawSizedAst(svg, child.tree, nextX, nextY);

            svg.appendChild(createLineSvgG(x, botOfHead, nextX, nextY));
        }
    }

    // サーバから受け取ったASTを描画
    // astは破壊的に変更されるため、再利用できない
    function drawAst(ast) {
        attachSizeToAst(ast);
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

    // 証明図にサイズ情報
    // (width, height, nodeWidth, nodeHeight, ruleWidthフィールド)を付加する
    function attachSizeToProofDiagram(diagram) {
        var nodeSize = getRichtextSize(diagram.typedExpr);
        diagram.nodeWidth = nodeSize.width;
        diagram.nodeHeight = nodeSize.height + textVerticalMargin*2;
        var ruleSize = getRichtextSize(diagram.rule);
        diagram.ruleWidth = ruleSize.width;

        if (diagram.parents.length == 0) {
            diagram.width = diagram.nodeWidth + ruleMargin + ruleSize.width;
            diagram.height = diagram.nodeHeight + ruleSize.height/2;
        } else {
            for (var i in diagram.parents)
                attachSizeToProofDiagram(diagram.parents[i])

            var widthOfParents = 0;
            for (var i in diagram.parents)
                widthOfParents += diagram.parents[i].width;
            widthOfParents += marginOfExpressions * (diagram.parents.length-1);

            var heightOfParents = 0;
            for (var i in diagram.parents)
                if (heightOfParents < diagram.parents[i].height)
                    heightOfParents = diagram.parents[i].height;

            var x = -widthOfParents / 2;
            for (var i in diagram.parents) {
                diagram.parents[i] = {
                    relativeX: x, // 左端
                    diagram: diagram.parents[i]
                }
                x += diagram.parents[i].diagram.width + marginOfExpressions;
            }

            var w = diagram.nodeWidth + ruleMargin + ruleSize.width;
            diagram.width = Math.max(w, widthOfParents);
            diagram.height = diagram.nodeHeight + lineWidth + heightOfParents;
        }
    }

    // サイズ情報の付いた証明図をsvg要素に描画
    // xは中央、yは上端
    function drawSizedProofDiagram(svg, diagram, x, y) {
        var left = x - (diagram.nodeWidth + ruleMargin + diagram.ruleWidth) / 2;
        svg.appendChild(richtextToSvgG(diagram.typedExpr, 'start',
                                       left, y + textVerticalMargin));
        var right = left + diagram.nodeWidth;
        for (var i in diagram.parents) {
            var parent = diagram.parents[i];
            var nextY = y - lineWidth - parent.diagram.nodeHeight;
            var nextX = x + parent.relativeX + parent.diagram.width/2;
            drawSizedProofDiagram(svg, parent.diagram, nextX, nextY);

            var newLeft = nextX - (parent.diagram.nodeWidth + ruleMargin +
                                   parent.diagram.ruleWidth) / 2
            var newRight = newLeft + parent.diagram.nodeWidth
            left = Math.min(left, newLeft)
            right = Math.max(right, newRight)
        }
        var barY = y - lineWidth/2;
        svg.appendChild(createLineSvgG(left, barY, right, barY));
        svg.appendChild(richtextToSvgG(diagram.rule, 'start',
                                       right + ruleMargin, barY));
    }

    // 証明図全体を描画
    // diagramは破壊的に変更されるため、再利用できない
    function drawProofDiagram(diagram, unification) {
        attachSizeToProofDiagram(diagram);
        attachSizeToUnification(unification);

        // 描画先であるsvg要素を作成
        var uWidth = 0;
        for (var i in unification) {
            if (uWidth < unification[i].width)
                uWidth =unification[i].width;
        }
        var uHeight = 0;
        for (var i in unification) {
            uHeight += unification[i].height;
        }
        var w = Math.max(diagram.width, uWidth) + svgMargin*2;
        var h = diagram.height + marginOfDiagramAndEquations +
                uHeight + svgMargin*2;
        var svgHtml = createSvgHtml("inference-svg", w, h);
        $('#inference-panel').html(svgHtml);

        // 描画
        var originX = svgMargin + diagram.width/2;
        var originY = svgMargin + diagram.height - diagram.nodeHeight;
        drawSizedProofDiagram($('#inference-svg').get(0), diagram, originX, originY);
        originX = svgMargin;
        for (var i in unification) {
            var x = unification[i].lhsWidth + unification[i].eqWidth/2 + svgMargin;
            if (originX < x)
                originX = x;
        }
        originY = svgMargin + diagram.height + marginOfDiagramAndEquations;
        drawUnification($('#inference-svg').get(0), unification, originX, originY)
    }

    function attachSizeToEquation(equation) {
        var lhsSize = getRichtextSize(equation.lhs);
        var eqSize = getRichtextSize(equation.eq);
        var rhsSize = getRichtextSize(equation.rhs);
        equation.lhsWidth = lhsSize.width;
        equation.eqWidth = eqSize.width + equationMargin*2;
        equation.rhsWidth = rhsSize.width;
        equation.width = equation.lhsWidth + equation.eqWidth ; equation.rhsWidth;
        equation.height = Math.max(lhsSize.height, eqSize.height, rhsSize.height);
        console.log(equation.height);
    }

    function attachSizeToUnification(unification) {
        for (var i in unification)
            attachSizeToEquation(unification[i]);
    }

    function drawEquation(svg, equation, eqCenterX, y) {
        var eqSize = getRichtextSize(equation.eq)
        var offset = eqSize.width/2 + equationMargin;
        var lhsG = richtextToSvgG(equation.lhs, "end", eqCenterX - offset, y)
        svg.appendChild(lhsG)
        var eqG = richtextToSvgG(equation.eq, "middle", eqCenterX, y)
        svg.appendChild(eqG)
        var rhsG = richtextToSvgG(equation.rhs, "start", eqCenterX + offset, y)
        svg.appendChild(rhsG)
    }

    function drawUnification(svg, unification, eqCenterX, y) {
        for (var i in unification) {
            drawEquation(svg, unification[i], eqCenterX, y);
            y += unification[i].height;
        }
    }

    // 式を入力するテキストボックスで
    // バックスラッシュと円マークをラムダに変換する処理
    // および、Enterキーが押されたら型付けボタンを押す処理
    $('#expression-input').keypress(function(event) {
        var enterKey = 13;
        var backslashChar = '\\'.charCodeAt(0);
        var yenChar = '¥'.charCodeAt(0);
        var keyCode = event.keyCode
        var charCode = event.charCode;
        if (keyCode == enterKey) {
            $('#type-button').click();
        } else if (charCode == backslashChar || charCode == yenChar) {
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
        'List Construction': '1::2::3::4::nil',
        'Arithmetic 1': '1+2*3-4',
        'Arithmetic 2': '(1-(-2))/3',
        'Logic 1': 'not (true equiv false)',
        'Logic 2': 'false or true and false',
        'Comparison': '1<2 and 3!=4'
    }
    $('#sample-selectpicker').on('change', function() {
        var key = $(this).find("option:selected").val();
        if (key in samplePrograms)
            $('#expression-input').val(samplePrograms[key]);
    });

    // 型付けボタンが押された時のAjax通信処理
    // http://ginpen.com/2013/05/07/jquery-ajax-form/
    $('#type-button').click(function(event) {
        $.ajax({
            url: '/typing',
            data: { expression: $('#expression-input').val() },
            timeout: 10000,
            dataType: 'json',

            // 送信前
            beforeSend: function(xhr, settings) {
                // ボタンを無効化し、二重送信を防止
                $('#type-button').attr('disabled', true);
            },
            // 応答後
            complete: function(xhr, textStatus) {
                // ボタンを有効化し、再送信を許可
                $('#type-button').attr('disabled', false);
            },

            // 通信成功時の処理
            success: function(json, textStatus, xhr) {
                if (json.parsed) {
                    drawExpr(json.expr);
                    drawPrincipalType(json.type);
                    drawAst(json.ast);
                    drawProofDiagram(json.proof, json.unification);
                    // test
                    // drawEquation($('#inference-svg').get(0), json.unification[0], 300, 300);
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
