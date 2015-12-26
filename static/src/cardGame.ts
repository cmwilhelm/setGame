import {Component}            from 'angular2/core';
import {HTTP_PROVIDERS, Http} from 'angular2/http';

interface ICard {
    cardId: number,
    color:  string,
    number: string,
    shade:  string,
    shape:  string
}

interface GameData {
    gameId?: number,
    cards:   Array<ICard>
}

const template = `
    <section>
      <ul>
	<li *ngFor="#card of gameData.cards" (click)="toggleSelection(card)">
          {{card.cardId}}
	</li>
      </ul>
    </section>
`;

@Component({
    selector:  'my-app',
    providers: [HTTP_PROVIDERS],
    template:  template
})
export class CardGameComponent {
    public gameData: GameData = {cards: []};
    public selected = {};
    public selectedCount: number = 0;

    private http: Http;

    constructor(http:Http) {
	this.http = http;
	this.initializeGameData();
    }

    public toggleSelection(card: ICard) {
	if (!this.isSelected(card)) {
	    this.selected[card.cardId] = true;
	    this.selectedCount += 1;
	} else {
	    delete this.selected[card.cardId];
	    this.selectedCount -= 1;
	}
	if (this.selectedCount === 3) {
	    this.checkSet();
	}
    }

    public isSelected(card: ICard): boolean {
	return this.selected[card.cardId] ? true : false;
    }

    private initializeGameData() {
	this.http.get('/sp/new').subscribe(response => {
	    const rawData = response.json();
	    this.updateGameData(rawData);
	});
    }

    private checkSet() {
	var selectedIds: Array<number> = [];
	for (var cardId in this.selected) {
	    selectedIds.push(parseInt(cardId));
	}
	console.log("testing ", selectedIds);
    }

    private updateGameData(rawData: Array<any>) {
	this.gameData = {gameId: rawData[0], cards: rawData[1]};
	this.selected = {};
	this.selectedCount = 0;
    }
}
