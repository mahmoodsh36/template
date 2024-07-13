function handleMathButton(node) {
  if (node.classList.contains('math-button')) {
    let symbol = node.children[0];
    symbol.setAttribute('original-fill', symbol.getAttribute('fill'));
    node.onmouseover = function() {
      node.children[0].setAttribute('fill', 'red');
    }
    node.onmouseout = function() {
      node.children[0].setAttribute('fill', symbol.getAttribute('original-fill'));
    }
  }
}

window.onload = function(e) {
  document.querySelectorAll(".math-button").forEach(elm => handleMathButton(elm));
}