import { marked } from 'marked';

const ALLOWED_TAGS = new Set([
  'a',
  'blockquote',
  'br',
  'code',
  'dd',
  'del',
  'div',
  'dl',
  'dt',
  'em',
  'h1',
  'h2',
  'h3',
  'h4',
  'h5',
  'h6',
  'hr',
  'img',
  'li',
  'ol',
  'p',
  'pre',
  'span',
  'strong',
  'table',
  'tbody',
  'td',
  'th',
  'thead',
  'tr',
  'ul',
]);

const ALLOWED_ATTRIBUTES = new Map([
  ['a', new Set(['href', 'title'])],
  ['img', new Set(['alt', 'src', 'title'])],
  ['code', new Set(['class'])],
]);

const URI_ATTRIBUTES = new Set(['href', 'src']);

marked.use({
  gfm: true,
  breaks: false,
});

function isAllowedUri(value) {
  const trimmed = String(value || '').trim();
  if (!trimmed) {
    return false;
  }
  if (
    trimmed.startsWith('#')
    || trimmed.startsWith('/')
    || trimmed.startsWith('./')
    || trimmed.startsWith('../')
  ) {
    return true;
  }
  try {
    const parsed = new URL(trimmed, 'https://rumoca.local/');
    return ['http:', 'https:', 'mailto:'].includes(parsed.protocol)
      || (parsed.protocol === 'data:' && /^data:image\/(?:png|jpe?g|gif|webp);/i.test(trimmed));
  } catch {
    return false;
  }
}

function sanitizeElement(element) {
  const tag = element.tagName.toLowerCase();
  if (!ALLOWED_TAGS.has(tag)) {
    element.replaceWith(...element.childNodes);
    return;
  }

  const allowedAttributes = ALLOWED_ATTRIBUTES.get(tag) || new Set();
  for (const attribute of [...element.attributes]) {
    const name = attribute.name.toLowerCase();
    if (!allowedAttributes.has(name)) {
      element.removeAttribute(attribute.name);
      continue;
    }
    if (URI_ATTRIBUTES.has(name) && !isAllowedUri(attribute.value)) {
      element.removeAttribute(attribute.name);
    }
  }

  if (tag === 'a') {
    element.setAttribute('rel', 'noreferrer noopener');
  }
}

function sanitizeTree(root) {
  const walker = document.createTreeWalker(root, NodeFilter.SHOW_ELEMENT | NodeFilter.SHOW_COMMENT);
  const nodes = [];
  while (walker.nextNode()) {
    nodes.push(walker.currentNode);
  }
  for (const node of nodes) {
    if (node.nodeType === Node.COMMENT_NODE) {
      node.remove();
    } else {
      sanitizeElement(node);
    }
  }
}

export function renderMarkdownHtml(markdown) {
  const template = document.createElement('template');
  template.innerHTML = marked.parse(String(markdown || ''));
  sanitizeTree(template.content);
  return template.innerHTML;
}
