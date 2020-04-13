const SEARCH_ENGINES = {
  GOOGLE: Symbol('Google'),
  DUCKDUCKGO: Symbol('DuckDuckGo')
};

const SEARCH_ENGINE = SEARCH_ENGINES.DUCKDUCKGO;
const SEARCH_DELAY = 50;
const KEY_CODES = {
  ARR_UP: 38,
  ARR_DN: 40,
  ARR_LT: 37,
  ARR_RT: 39,
  K: 75,
  J: 74,
  H: 72,
  L: 76,
  ENTER: 13,
  ESC: 27,
  SPACE: 32,
  TAB: 9
};

function qs(selector, scope) {
  return (scope || document).querySelector(selector);
}

function debounce(func, time) {
  return () => {
    clearTimeout(func.timeout);
    func.timeout = setTimeout(func, time);
  }
}

function $on(element, type, callback) {
  element.addEventListener(type, callback);
}

function $delegate(element, type, selector, callback) {
  const dispatchEvent = event => {
    const targetElement = event.target;
    const potentialElements = element.querySelectorAll(selector);
    let i = potentialElements.length;

    while (i--) {
      if (potentialElements[i] === targetElement) {
        callback.call(targetElement, event);
        break;
      }
    }
  };

  $on(element, type, dispatchEvent);
}

class SearchView {
  constructor() {
    this.suggestionTemplate = qs('#suggestion-template');
    this.$search = qs('#search');
    this.$suggestions = qs('#suggestions');
  }

  setQuery(value) {
    this.$search.value = value;
  }

  getQuery() {
    return this.$search.value;
  }

  showSuggestions(suggestions) {
    const suggestionsHTML = suggestions.map(suggestion => `<div class="js-suggestion search__suggestion">${suggestion}</div>`).join('');
    this.$suggestions.innerHTML = suggestionsHTML;
    this.$suggestions.classList.add('has-suggestions');
  }

  clearSuggestions() {
    this.$suggestions.classList.remove('has-suggestions');
    this.$suggestions.innerHTML = '';
  }

  hasSuggestions() {
    return this.$suggestions.classList.contains('has-suggestions');
  }

  focusPreviousSuggestion() {
    const selectedSuggestion = qs('.is-focused', this.$suggestions) || this.$suggestions.firstElementChild;
    const prevEl = selectedSuggestion.previousElementSibling || selectedSuggestion.parentElement.lastElementChild;

    selectedSuggestion.classList.remove('is-focused');
    prevEl.classList.add('is-focused');
    return prevEl.innerHTML;
  }

  focusNextSuggestion() {
    const selectedSuggestion = qs('.is-focused', this.$suggestions) || this.$suggestions.lastElementChild;
    const nextEl = selectedSuggestion.nextElementSibling || selectedSuggestion.parentElement.firstElementChild;

    selectedSuggestion.classList.remove('is-focused');
    nextEl.classList.add('is-focused');
    return nextEl.innerHTML;
  }

  bindSearchKeyDown(handler) {
    $on(this.$search, 'keydown', handler);
  }

  bindSearchInput(handler) {
    $on(this.$search, 'input', debounce(handler, SEARCH_DELAY));
  }

  bindSuggestionClick(handler) {
    $delegate(this.$suggestions, 'click', '.js-suggestion', handler);
  }
}

class GoogleSearchService {
  set responseHandler(handler) {
    this.handler = handler;
  }

  fetchSearchSuggestions(query) {
    const suggestionScriptEl = document.createElement('script');
    suggestionScriptEl.src = `https://www.google.com/complete/search?client=firefox&format=json&callback=autocompleteCallback&hl=en&q=${encodeURIComponent(query)}`;
    document.body.appendChild(suggestionScriptEl);
    document.body.removeChild(suggestionScriptEl);
  }

  handleSuggestionsResponse(response) {
    const query = response[0];
    const suggestions = response[1] || [];
    this.handler(suggestions);
  }

  search(query) {
    document.location.href = `https://www.google.com/search?q=${encodeURIComponent(query)}`;
  }
}

class DuckDuckGoSearchService {
  set responseHandler(handler) {
    this.handler = handler;
  }

  fetchSearchSuggestions(query) {
    const suggestionScriptEl = document.createElement('script');
    suggestionScriptEl.src = `https://duckduckgo.com/ac?callback=autocompleteCallback&q=${query}`;
    document.body.appendChild(suggestionScriptEl);
    document.body.removeChild(suggestionScriptEl);
  }

  handleSuggestionsResponse(response) {
    const suggestions = response.map(suggestion => suggestion.phrase);
    this.handler(suggestions);
  }

  search(query) {
    document.location.href = `https://duckduckgo.com/?q=${encodeURIComponent(query)}`;
  }
}

class SearchController {
  constructor(searchView, searchService) {
    this.searchView = searchView;
    this.searchService = searchService;

    this.searchView.bindSearchKeyDown(this.searchKeyDownHandler.bind(this));
    this.searchView.bindSearchInput(this.searchInputHandler.bind(this));
    this.searchView.bindSuggestionClick(this.suggestionClickHandler.bind(this));

    this.searchService.responseHandler = this.suggestionsResponseHandler.bind(this);
  }

  suggestionsResponseHandler(suggestions) {
    if (suggestions.length) {
      this.searchView.showSuggestions(suggestions);
    } else {
      this.searchView.clearSuggestions();
    }
  }

  searchKeyDownHandler(e) {
    const query = this.searchView.getQuery().trim();
    const keyCode = e.keyCode;

    if (keyCode === KEY_CODES.ENTER && query) {
      this.search(query);
    } else if (keyCode === KEY_CODES.TAB || keyCode === KEY_CODES.ESC) {
      this.searchView.clearSuggestions();
    } else if (this.searchView.hasSuggestions()) {
      if (keyCode === KEY_CODES.ARR_UP) {
        e.preventDefault();
        this.searchView.setQuery(this.searchView.focusPreviousSuggestion());
      } else if (keyCode === KEY_CODES.ARR_DN) {
        e.preventDefault();
        this.searchView.setQuery(this.searchView.focusNextSuggestion());
      }
    }
  }

  searchInputHandler(e) {
    const query = this.searchView.getQuery().trim();
    if (query) {
      this.searchService.fetchSearchSuggestions(query);
    } else {
      this.searchView.clearSuggestions();
    }
  }

  suggestionClickHandler(e) {
    this.search(e.target.innerHTML);
  }

  search(query) {
    this.searchService.search(query);
  }
}

let searchService;
switch (SEARCH_ENGINE) {
  case SEARCH_ENGINES.GOOGLE:
    searchService = new GoogleSearchService();
    break;
  case SEARCH_ENGINES.DUCKDUCKGO:
    searchService = new DuckDuckGoSearchService();
    break;
  default:
    break;
}

const searchView = new SearchView();
const searchController = new SearchController(searchView, searchService);
const autocompleteCallback = searchService.handleSuggestionsResponse.bind(searchService);

qs('#links').addEventListener('keydown', (e) => {
  const keyCode = e.keyCode;
  const target = e.target;
  const origIndex = Array.from(target.parentElement.children).indexOf(target);

  switch (keyCode) {
    // UP
    case KEY_CODES.ARR_UP:
    case KEY_CODES.K:
      const prevEl = target.previousElementSibling || target.parentElement.lastElementChild;
      prevEl.focus();
      break;

    // DOWN
    case KEY_CODES.ARR_DN:
    case KEY_CODES.J:
      const nextEl = target.nextElementSibling || target.parentElement.firstElementChild;
      nextEl.focus();
      break;

    // LEFT
    case KEY_CODES.ARR_LT:
    case KEY_CODES.H:
      const prevRubricEl = target.parentElement.previousElementSibling || target.parentElement.parentElement.lastElementChild;
      prevRubricEl.children[Math.min(origIndex, prevRubricEl.children.length - 1)].focus();
      break;

    // RIGHT
    case KEY_CODES.ARR_RT:
    case KEY_CODES.L:
      const nextRubricEl = target.parentElement.nextElementSibling || target.parentElement.parentElement.firstElementChild;;
      nextRubricEl.children[Math.min(origIndex, nextRubricEl.children.length - 1)].focus();
      break;

    case KEY_CODES.SPACE:
      target.click();
      break;
  }
});
