System.register(['angular2/core', 'angular2/http'], function(exports_1) {
    var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
        var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
        if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
        else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
        return c > 3 && r && Object.defineProperty(target, key, r), r;
    };
    var __metadata = (this && this.__metadata) || function (k, v) {
        if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
    };
    var core_1, http_1;
    var template, CardGameComponent;
    return {
        setters:[
            function (core_1_1) {
                core_1 = core_1_1;
            },
            function (http_1_1) {
                http_1 = http_1_1;
            }],
        execute: function() {
            template = "\n    <section>\n      <ul>\n\t<li *ngFor=\"#card of gameData.cards\" (click)=\"toggleSelection(card)\">\n          {{card.cardId}}\n\t</li>\n      </ul>\n    </section>\n";
            CardGameComponent = (function () {
                function CardGameComponent(http) {
                    this.gameData = { cards: [] };
                    this.selected = {};
                    this.selectedCount = 0;
                    this.http = http;
                    this.initializeGameData();
                }
                CardGameComponent.prototype.toggleSelection = function (card) {
                    if (!this.isSelected(card)) {
                        this.selected[card.cardId] = true;
                        this.selectedCount += 1;
                    }
                    else {
                        delete this.selected[card.cardId];
                        this.selectedCount -= 1;
                    }
                    if (this.selectedCount === 3) {
                        this.checkSet();
                    }
                };
                CardGameComponent.prototype.isSelected = function (card) {
                    return this.selected[card.cardId] ? true : false;
                };
                CardGameComponent.prototype.initializeGameData = function () {
                    var _this = this;
                    this.http.get('/sp/new').subscribe(function (response) {
                        var rawData = response.json();
                        _this.updateGameData(rawData);
                    });
                };
                CardGameComponent.prototype.checkSet = function () {
                    var selectedIds = [];
                    for (var cardId in this.selected) {
                        selectedIds.push(parseInt(cardId));
                    }
                    console.log("testing ", selectedIds);
                    //this.http.post();
                };
                CardGameComponent.prototype.updateGameData = function (rawData) {
                    this.gameData = { gameId: rawData[0], cards: rawData[1] };
                    this.selected = {};
                    this.selectedCount = 0;
                };
                CardGameComponent = __decorate([
                    core_1.Component({
                        selector: 'my-app',
                        providers: [http_1.HTTP_PROVIDERS],
                        template: template
                    }), 
                    __metadata('design:paramtypes', [http_1.Http])
                ], CardGameComponent);
                return CardGameComponent;
            })();
            exports_1("CardGameComponent", CardGameComponent);
        }
    }
});
//# sourceMappingURL=cardGame.js.map