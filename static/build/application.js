System.register(['angular2/platform/browser', './cardGame'], function(exports_1) {
    var browser_1, cardGame_1;
    return {
        setters:[
            function (browser_1_1) {
                browser_1 = browser_1_1;
            },
            function (cardGame_1_1) {
                cardGame_1 = cardGame_1_1;
            }],
        execute: function() {
            browser_1.bootstrap(cardGame_1.CardGameComponent);
        }
    }
});
//# sourceMappingURL=application.js.map