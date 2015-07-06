jQuery(function($) {
    var canvasFont = "30px Times New Roman";
    var textHeight = 30;

    var astCanvas = document.getElementById("ast-canvas");
    var astCtx = astCanvas.getContext("2d");
    astCtx.font = canvasFont;

    var drawAst = function(ast) {
        var astJson = JSON.parse(ast)["ast"]

        // astを、テキスト（大きさの情報を持つ）のツリーに変換
        var transform1 = function(json) {
            if (typeof(json) === "string") {
                var dimension = astCtx.measureText(json);
                return {
                    kind: "text",
                    text: json,
                    width: dimension.width,
                }
            } else {
                var head = transform1(json[0]);
                var children = new Array()
                for (var i = 1; i < json.length; i++)
                    children[i-1] = transform1(json[i]);
                return {
                    kind: "tree",
                    head: head,
                    children: children
                }
            }
        }
        sizedTexts = transform1(astJson)

        // 各部分木にサイズ情報を付加する
        var marginOfSubtrees = 10;
        var edgeHeight = 20;

        // originXは中央、originYは上端
        var transform2 = function(obj) {
            if (obj.kind == "text") {
                return {
                    kind: "text",
                    text: obj.text,
                    width: obj.width,
                    height: textHeight
                }
            } else {
                var children = new Array();
                for (var i in obj.children) {
                    children[i] = transform2(obj.children[i]);
                }
                var widthOfChildren = 0
                for (var i in children) {
                    widthOfChildren += children[i].width;
                }
                widthOfChildren += marginOfSubtrees * (children.length-1);
                var heightOfChildren = 0;
                for (var i in children) {
                    if (heightOfChildren < children[i].height) {
                        heightOfChildren = children[i].height;
                    }
                }
                var x = -widthOfChildren / 2;
                for (var i in children) {
                    children[i] = {
                        relativeX: x, // 左端
                        tree: children[i]
                    }
                    x += children[i].tree.width + marginOfSubtrees;
                }
                head = transform2(obj.head);
                return {
                    kind: "tree",
                    head: head,
                    children: children,
                    width: Math.max(head.width, widthOfChildren),
                    height: head.height + edgeHeight + heightOfChildren
                }
            }
        }
        var drawableTree = transform2(sizedTexts);
        console.log(drawableTree);

        // キャンバスのサイズをセット
        var canvasMargin = 10;
        astCtx.canvas.width = drawableTree.width + canvasMargin*2;
        astCtx.canvas.height = drawableTree.height + canvasMargin*2;
        astCtx.font = canvasFont;
        astCtx.textBaseline = "top";
        astCtx.textAlign = "center";
        astCtx.lineWidth = 2;

        // キャンバスに描画
        // xは中央、yは上端
        var draw = function(obj, x, y) {
            if (obj.kind == "text") {
                astCtx.fillText(obj.text, x, y);
            } else {
                draw(obj.head, x, y);
                var botOfHead = y + obj.head.height;
                var nextY = botOfHead + edgeHeight;
                for (var i in obj.children) {
                    var node = obj.children[i];
                    var nextX = x + node.relativeX + node.tree.width/2;
                    draw(node.tree, nextX, nextY);
                    astCtx.moveTo(x, botOfHead);
                    astCtx.lineTo(nextX, nextY);
                    astCtx.stroke();
                }
            }
        }
        draw(drawableTree, canvasMargin + drawableTree.width/2, canvasMargin);
    }

    // http://ginpen.com/2013/05/07/jquery-ajax-form/
    $('#expression-form').submit(function(event) {
        // HTMLでの送信をキャンセル
        event.preventDefault();

        // 操作対象のフォーム要素を取得
        var $form = $(this);

        // 送信ボタンを取得
        // （後で使う: 二重送信を防止する。）
        var $button = $form.find('button');

        // 送信
        $.ajax({
            url: $form.attr('action'),
            type: $form.attr('method'),
            data: $form.serialize(),
            timeout: 10000,  // 単位はミリ秒

            // 送信前
            beforeSend: function(xhr, settings) {
                // ボタンを無効化し、二重送信を防止
                $button.attr('disabled', true);
            },
            // 応答後
            complete: function(xhr, textStatus) {
                // ボタンを有効化し、再送信を許可
                $button.attr('disabled', false);
            },

            // 通信成功時の処理
            success: function(result, textStatus, xhr) {
                drawAst(result);
            },

            // 通信失敗時の処理
            error: function(xhr, textStatus, error) {
                $('#ast').text(textStatus);
            }
        });
    });
});
