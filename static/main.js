// search data
let data = [];

window.onload = function(e) {
  // make all "math buttons" clickable
  document.querySelectorAll(".fancy-button").forEach(elm => handleMathButton(elm));
  // fetch the search data
  fetch("search.json").then(response => response.json()).then(json => {
    for (let entry of json) {
      let entryText = entry.id || entry.title;
      if (entryText !== undefined && entryText !== null) {
        data.push(entry);
      }
    }
    search('');
  });
  document.querySelectorAll('svg').forEach(entry => randomizeSvgIds(entry)); // fix some rendering issues caused by conflicting id's

  // sometimes org inserts redundant <br> that cause annoying visual breaks, gets rid of those
  function removeTrailingBr(element) {
    while (element.lastChild && element.lastChild.tagName === 'BR') {
      element.removeChild(element.lastChild);
    }
  }
  document.querySelectorAll('*').forEach(removeTrailingBr);

  // fancy blocks :D
  fixFancyBlocks();

  // for feather icons
  feather.replace();

  // i configured org to preserve linebreaks but it preserves those after svg.org-latex-block which results in weird spaces, get rid of those
  document.querySelectorAll('svg.org-latex-block').forEach((elm) => {
    let next = elm.nextSibling;
    if (next && next.tagName === 'BR')
      next.remove();
  });
}

// when loading an svg from another page, we need to modify the id's that it uses for the elements to avoid conflicts with svg's we already have on the current page which may have similar ids and cause rendering issues.
function randomizeSvgIds(node, idsMap=new Map()) {
  randomizeSvgIdsHandleElement(node, idsMap);
  for (var i = 0; i < node.childNodes.length; i++) {
    var child = node.childNodes[i];
    randomizeSvgIds(child, idsMap);
    randomizeSvgIdsHandleElement(child, idsMap);
  }
}
function randomizeSvgIdsHandleElement(elm, idsMap) {
  if (elm.id) {
    // create new id if it doesnt exist in the map
    if (!idsMap.has(elm.id))
      idsMap.set(elm.id, randomString(10));
    elm.id = idsMap.get(elm.id);
  }
  if (elm.href !== undefined) {
    // modify id reference
    let myId = elm.href.baseVal.substr(1);
    if (myId) {
      if (!idsMap.has(myId))
        idsMap.set(myId, randomString(10));
      elm.href.baseVal = '#' + idsMap.get(myId);
    }
  }
}
// from https://stackoverflow.com/questions/1349404/generate-random-string-characters-in-javascript
function randomString(length) {
  let result = '';
  const characters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
  const charactersLength = characters.length;
  let counter = 0;
  while (counter < length) {
    result += characters.charAt(Math.floor(Math.random() * charactersLength));
    counter += 1;
  }
  return result;
}

let popupElm;
let toShow = true; /* we need this because by the time the web request is done the user might've moved the mouse and we no longer need to show the result */
// so a "fancy button" is something that has a data-ref property and .fancy-button class
function handleMathButton(node) {
  let ref = node.getAttribute('data-ref');
  let refId = ref.substr(4);

  // we can put something together with this to make the area bigger
  // <rect onmouseover="alert('hi')" width="200" height="100" x="10" y="10" rx="20" ry="20" fill="transparent"/>

  if (node.classList.contains('fancy-button')) {
    let symbol = node.children[0];
    symbol.setAttribute('original-fill', symbol.getAttribute('fill'));
    node.onmouseover = function(event) {
      toShow = true;
      if (popupElm !== undefined)
        popupElm.innerHTML = ''; // clear previously added blocks
      symbol.setAttribute('fill', 'red');
      getElementByBlkId(refId, function (elm) {
        if (!toShow)
          return;
        if (!elm)
          return;
        console.log(refId);
        // elm = elm.cloneNode(true); // clone it so we wont have problems
        elm.querySelectorAll('svg').forEach(entry => randomizeSvgIds(entry)); // fix some rendering issues caused by conflicting id's
        if (popupElm === undefined) {
          popupElm = document.createElement('div');
          popupElm.className = "popup";
          document.body.appendChild(popupElm);
        }
        popupElm.innerHTML = '';
        popupElm.appendChild(elm);
        Object.assign(popupElm.style, {
          left: `${event.pageX}px`,
          top:  `${event.pageY}px`,
          display: `block`,
        });
      });
    }
    node.onmouseout = function() {
      toShow = false;
      symbol.setAttribute('fill', symbol.getAttribute('original-fill'));
      if (popupElm !== undefined) {
        popupElm.innerHTML = '';
        Object.assign(popupElm.style, {
          display: `none`,
        });
      }
    }
    node.onclick = function() {
      let entry = findById(refId);
      // window.open(entry.filepath);
      location.href = entry.filepath;
    }
  }
}

// find a blk entry by its id
function findById(id) {
  for (let entry of data) {
    if (entry.id !== null && entry.id === id) {
      return entry;
    }
  }
}

function getElementByBlkId(id, cb) {
  let entry = findById(id);
  if (entry !== undefined) {
    fetch(entry.filepath).then(response => response.text()).then(function(text) {
      // parse the "other" page (page containing the destination entry)
      let page = new DOMParser().parseFromString(text, "text/html");
      // the actual html entry from the other page
      let elm = page.getElementById(entry.id);
      document.a = elm;
      if (elm && elm.parentElement.classList.contains('fancy-container')) {
        cb(elm.parentElement)
      } else {
        cb(elm);
      }
    });
  }
}

function searchInput(el) {
  search(el.value);
}

function search(val) {
  let resultsContainer = document.getElementById("search-results-container");
  resultsContainer.innerHTML = '';
  // if (val === '')
  //   return;
  let matchingEntries = [];
  for (let entry of data) {
    const entryText = entry.title || entry.id;
    if (entryText && entryText.includes(val)) {
      const container = document.createElement("div");
      const subcontainer = document.createElement("div");
      const span = document.createElement("span");
      const plusMinusButton = document.createElement("div");
      const infoElm = document.createElement("div");

      container.className = 'search-result-container';
      plusMinusButton.className = 'plus-button';
      infoElm.className = 'info';
      subcontainer.className = 'search-result';

      // on-demand info of reference/page/whatever
      plusMinusButton.onclick = function(elm) {
        // so that we dont insert duplicate info
        infoElm.innerHTML = '';

        console.log(entry);
        let isPlus = plusMinusButton.className === 'plus-button';

        if (isPlus) {
          plusMinusButton.className = 'minus-button';
          fetch(entry.filepath).then(response => response.text()).then(function(text) {
            // parse the "other" page (page containing the destination entry)
            let page = new DOMParser().parseFromString(text, "text/html");
            // the actual html entry from the other page
            let docElm = page.getElementById(entry.id);
            // the type of the entry
            let mytype;

            // direct link to the entry in its parent page
            let mylink = entry['filepath'];
            if (docElm !== null)
              mylink = mylink + '#' + entry.id;
            let linkElm = document.createElement('a');
            linkElm.href = mylink;
            linkElm.innerHTML = 'direct link';

            let topRow = document.createElement('div');
            topRow.className = 'separated-row';

            // insert the info
            if (entry['matched-pattern']['shared-name'] === 'blk-org-file-rule')
              mytype = 'document';
            if (docElm !== null)
              mytype = 'reference';
            if (mytype)
              topRow.appendChild(document.createTextNode('type: ' + mytype));
            else
              topRow.appendChild(document.createTextNode('empty'));

            // insert the on-demand info elements into the dom
            topRow.appendChild(linkElm);
            infoElm.appendChild(topRow);
            if (docElm !== null)
              infoElm.appendChild(docElm);
            container.appendChild(infoElm);
          });
        } else {
          plusMinusButton.className = 'plus-button';
          container.querySelector('.info').remove();
        }
      }

      span.appendChild(document.createTextNode(entryText));
      container.appendChild(subcontainer);
      subcontainer.appendChild(span);
      subcontainer.appendChild(plusMinusButton);
      container.appendChild(infoElm);
      resultsContainer.appendChild(container);

      matchingEntries.push(entry);
    }
  }
  // update numbers
  document.getElementById("search-numbers-results").innerHTML = '' + matchingEntries.length;
  document.getElementById("search-numbers-public").innerHTML = '' + data.length;
}

function copyAnchor(elm) {
  const href = elm.getAttribute('href');
  const toCopy = location.origin + location.pathname + href;
  navigator.clipboard.writeText(toCopy)
    .then(() => {
      console.log(toCopy);
    });
}

function fixFancyBlocks() {
  let fancyBlocks = document.querySelectorAll('.fancy-block');
  let handled = [];
  for (let fancyBlock of fancyBlocks) {
    if (handled.includes(fancyBlock))
      continue;
    let parent = fancyBlock.parentNode;
    let container = document.createElement('div');
    let before = document.createElement('div');

    container.className = 'fancy-container';

    // fancy-before
    before.className = 'fancy-before';
    before.innerHTML = fancyBlock.getAttribute('data-before');

    // fancy-after
    let after = document.createElement('div');
    after.className = 'fancy-after';
    after.innerHTML = fancyBlock.getAttribute('data-after');

    // anchor link button
    let link = document.createElement('a');
    let anchor = fancyBlock.getAttribute('data-id');
    link.href = anchor;
    link.className = 'copy-btn feather-16'
    link.onclick = () => { copyAnchor(this); };
    link.setAttribute('data-feather', "link");

    // set the wrapper as child (instead of the element)
    parent.replaceChild(container, fancyBlock);
    if (anchor)
      container.appendChild(link);
    container.appendChild(before);
    container.appendChild(fancyBlock);
    container.appendChild(after);

    handled.push(fancyBlock);
  }
}