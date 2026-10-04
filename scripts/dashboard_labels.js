(function(){
 function rename(value){return value.replace(/(localisations? du projet)|\bprojets?\b/gi,function(word,done){if(done)return word;var plural=/s$/i.test(word);var text=plural?'localisations du projet':'localisation du projet';return /^[A-Z]/.test(word)?text.charAt(0).toUpperCase()+text.slice(1):text;});}
 function visit(node){
  if(node.nodeType===3){if(node.parentElement&&!/^(SCRIPT|STYLE|TEXTAREA)$/.test(node.parentElement.tagName)){var text=rename(node.nodeValue);if(text!==node.nodeValue)node.nodeValue=text;}return;}
  if(node.nodeType!==1||/^(SCRIPT|STYLE|TEXTAREA)$/.test(node.tagName))return;
  ['title','aria-label'].forEach(function(a){if(node.hasAttribute(a)){var old=node.getAttribute(a),text=rename(old);if(text!==old)node.setAttribute(a,text);}});
  Array.from(node.childNodes).forEach(visit);
 }
 visit(document.body);document.title=rename(document.title);
 new MutationObserver(function(changes){changes.forEach(function(c){if(c.type==='characterData')visit(c.target);else c.addedNodes.forEach(visit);});}).observe(document.body,{subtree:true,childList:true,characterData:true});
})();
