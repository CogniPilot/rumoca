// Minimal element builder shared by the assistant UI modules.
export function el(tag, attributes = {}, ...children) {
    const node = document.createElement(tag);
    for (const [name, value] of Object.entries(attributes)) {
        if (value === undefined || value === null || value === false) continue;
        if (name === 'class') node.className = value;
        else if (name.startsWith('on')) node.addEventListener(name.slice(2), value);
        else node.setAttribute(name, value === true ? '' : value);
    }
    for (const child of children.flat()) {
        if (child === undefined || child === null || child === false) continue;
        node.append(child instanceof Node ? child : document.createTextNode(String(child)));
    }
    return node;
}
