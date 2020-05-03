/*
 * Przystosowuje las do schematu ktorego oczekuje viewer.js
 * viewer.js bedzie wywolywac rozne funkcje z tego pliku w miare potrzeby
 */

function helper_prepare(tree) {
	var f = new forest(tree.node, tree.startnode.nid);

	for (var k in tree)
		if (k != "node" && k != "startnode")
			f[k] = tree[k];

	/* Wezel rozstrzygany potrzebuje dodatkowego miejsca na znak
	 * zapytania */
	if ("disamb_node_id" in tree && f.nodes[tree.disamb_node_id].leaf)
		f.nodes[tree.disamb_node_id].space = 0.4;

	/* Terminale nie sa wyswietlane */
	function hide_terminals(node) {
		if (node.terminal) {
			node.hidden = true;
			return;
		}
		if (node.leaf)
			node.incomplete = true;
		if (!node.children)
			return;

		for (var c in node.children)
			for (var ch in node.children[c].child)
				hide_terminals(node.children[c].child[ch]);
	}
	hide_terminals(f.root);

	return f;
}

function helper_prepare_filtered(forest) {
	forest.root.update_depth = function() {
		for (var nid in forest.nodes)
			forest.nodes[nid].depth = [ 0, 0, 0, 0 ];
	}

	forestnode.prototype.show_filtered = function(viewer) {
	}
}

function helper_nodecopy(node) {
	var newnode = {};
	for (var i in node)
		newnode[i] = node[i];
	return newnode;
}

function helper_get_subtree(tree, n, diffattrs) {
	/* Can't use \w -- Firefox seems to only understand ascii */
	diffattrs = eval(diffattrs.replace(/[^[\] ,]+/gi, "\"$&\""));

	var nodes = {};
	var root = helper_nodecopy(tree.node[tree.disamb_node_id]);
	var child = root.children[n];

	root.hidden = true;
	root.incomplete = false;
	root.leaf = false;
	root.current = 0;
	if ("space" in root)
		delete root["space"];
	root.children = [ {
			heads: child.heads,
			"child": [],
			rule: child.rule,
		} ];

	nodes[root.nid] = root;
	for (var i in child.child) {
		var nid = child.child[i].nid;
		var subnode = helper_nodecopy(tree.node[nid]);

		subnode.hidden = !!subnode.terminal;
		subnode.incomplete = !subnode.terminal/* && !(
			subnode.children[0].child.length == 1 &&
			subnode.children[0].child[0].terminal)*/;
		subnode.leaf = true;
		subnode.show_attribs = true;
		subnode.diffattrs = {};
		for (var j in diffattrs[i])
			subnode.diffattrs[diffattrs[i][j]] = true;

		delete subnode.children;
		nodes[nid] = subnode;
		root.children[0].child.push(child.child[i]);
	}

	var subtree = new forest(nodes, root.nid);
	for (var k in tree)
		if (k != "node" && k != "startnode")
			subtree[k] = tree[k];

	subtree.stats = eval(uneval(subtree.stats));
	subtree.stats.trees = root.subtrees;

	return subtree;
}

function get_topleft(node, parent) {
	/* Firefox 3+ */
	if (node.getBoundingClientRect != undefined) {
		if (parent == null)
			parent = document.body;

		var rect = node.getBoundingClientRect();
		var prect = parent.getBoundingClientRect();
		return [ rect.left - prect.left, rect.top - prect.top ];
	}

	ret = [ node.clientLeft - document.body.clientLeft,
		node.clientTop - document.body.clientTop ];
	for (; node != parent; node = node.offsetParent)
		ret = [ ret[0] + node.offsetLeft - node.scrollLeft,
			ret[1] + node.offsetTop - node.scrollTop ];

	ret[0] += 2; /* No way to easily calculate border widths :( */
	ret[1] += 2; /* No way to easily calculate border widths :( */

	return ret;
}

function helper_update_node_info(node, onover, onout, onwheel, onswitch,
		onresize, add_attr_spans, nopopup) {
	var text = node.terminal ? "\"" + node.terminal.base + "\"" :
			node.nonterminal.category;
	node.info.innerHTML = "";

	var inactive = node.inactive || (node.orig != undefined &&
			node.orig != node.current && node.chtype);
	node.elem = document.createElement("div");
	node.elem.innerHTML = text.to_xml_safe();
	node.elem.className = inactive ? "inactive-node" :
		node.terminal ? "terminal-node" :
		!node.chtype ? "one-non-suggested-node" :
		node.chtype == 1 ? "unresolved-node" :
		node.chtype == 2 ? "many-suggested-node" :
		node.chtype == 3 ? "one-suggested-node" :
		node.chtype == 4 ? "user-suggested-node" :
		"user-non-suggested-node";
	if ("target" in node)
		node.elem.className += " link";
	if (node.selected) {
		node.elem.className += " nonterminal-node-active";
		node.info.className += " nonterminal-node-active";
	}
	node.elem.className += " nodecontent";

	var m = document.createElement("div");
	m.className = "middle";
	var mi = document.createElement("div");
	mi.className = "inner";

	mi.appendChild(node.elem);
	m.appendChild(mi);
	node.info.appendChild(m);

	if ("target" in node)
		attach(node.elem, "click", function(evt) {
					nopopup();
					var id = "main:accessibleNodes:" +
						node.target + ":nodebutton";
					document.getElementById(id).click();
				}, false);

	if (!node.hidden && !node.incomplete && node.children) {
		var rulename = "";
		if (node.children[node.current].rule)
			rulename =
				node.children[node.current].rule.to_xml_safe();

		if (node.orig == undefined)
			node.orig = node.current;

		for (var chld in node.current_children)
			node.current_children[chld].inactive = inactive;

		var rule = document.createElement("span");
		rule.className = "rule";

		if (node.current) {
			var img = document.createElement("img");
			img.setAttribute("src", "img/gtk-left.png");
			img.setAttribute("width", "12");
			img.setAttribute("height", "12");
			img.onclick = function() {
				onswitch(-1);
			};

			rule.appendChild(img);
		}

		rule.appendChild(document.createTextNode(rulename));

		if (node.current < node.children.length - 1) {
			var img = document.createElement("img");
			img.setAttribute("src", "img/gtk-right.png");
			img.setAttribute("width", "12");
			img.setAttribute("height", "12");
			img.onclick = function() {
				onswitch(1);
			};

			rule.appendChild(img);
		}

		m.appendChild(rule);

		attach(rule, "DOMMouseScroll", onwheel, false);
	}

	var twin = null;
	var dummy = null;
	var attrdisplay = document.createElement("div");
	node.elem.appendChild(attrdisplay);

	var updateattrdisplay = function() {
		var on = node.show_attribs || twin != null || node.pinned;

		attrdisplay.innerHTML = "";

		var tmr;
		if (!on) {
			attach(node.elem, "mouseover",
					function(evt) { tmr = setTimeout(
					function() { customonover(evt); },
					400); }, false);
			attach(node.elem, "mouseout",
					function(evt) { clearTimeout(tmr); },
					false);
			return;
		}

		if (node.show_attribs) {
			attach(node.elem, "mouseover", onover, false);
			attach(node.elem, "mouseout", onout, false);
		} else if (twin != null) {
			/* The timer works around what seems to be a Gecko
			 * bug */
			attach(node.elem, "mouseover",
					function(evt) { clearTimeout(tmr); },
					false);
			attach(node.elem, "mouseout",
					function(evt) { tmr = setTimeout(
					function() { customonout(evt); },
					10); }, false);
		} else {
			attach(node.elem, "mouseover",
					function(evt) { tmr = setTimeout(
					function() { customonover(evt); },
					400); }, false);
			attach(node.elem, "mouseout",
					function(evt) { clearTimeout(tmr); },
					false);
		}

		helper_fill_node_attributes(node, attrdisplay, add_attr_spans);

		if (twin == null)
			return;

		var pin = document.createElement("img");
		pin.setAttribute("alt", "Przypnij węzeł");
		pin.setAttribute("title", "Przypnij węzeł");
		/*pin.style.cssFloat = "right";*/
		var pinmouse = function(state) {
			pin.setAttribute("src", [ "img/unpinned.png",
						"img/pinned.png" ]
					[node.pinned ^ state]);
		};
		pin.onmouseover = function() { pinmouse(1) };
		pin.onmouseout = function() { pinmouse(0) };
		pinmouse(0);

		var sp = document.createElement("div");
		sp.style.textAlign = "right";
		sp.appendChild(pin);
		attrdisplay.appendChild(sp);

		pin.onclick = function(evt) {
			node.pinned = !node.pinned;

			/* Try all the possible ways to stop
			 * event propagation */
			if (evt.stopPropagation)
				evt.stopPropagation();
			if (evt.preventDefault)
				evt.preventDefault();
			evt.cancelBubble = true;
			evt.returnValue = false;

			if (!node.pinned) {
				customonout(evt);
				onresize();

				return false;
			}

			/* Temporarily remove to get the right size */
			attrdisplay.removeChild(sp);
			dummy.style.width = node.elem.offsetWidth + "px";
			dummy.style.height = node.elem.offsetHeight + "px";

			/* Tell the viewer to relayout everything */
			onresize();
			attrdisplay.appendChild(sp);

			/* Assuming mouse is "over" when clicking */
			pinmouse(0);

			return false;
		};
	}

	var parent = document.getElementById("viewer");
	var customonover = function(evt) {
		if (twin != null)
			return;

		if (!node.attrs_order.length)
			return;

		var pos = get_topleft(node.info,
				parent.firstChild.offsetParent);
		var size = [ node.info.offsetWidth, node.info.offsetHeight ];

		twin = document.createElement("div");
		twin.className = node.info.className;
		twin.style.cssText = node.info.style.cssText;
		twin.style.position = "absolute";
		twin.style.left = (pos[0] - 100) + "px";
		twin.style.top = pos[1] + "px";
		twin.style.width = (size[0] + 200) + "px";
		twin.style.zIndex = "1000";

		var m2 = document.createElement("div");
		m2.className = m.className;
		m2.style.cssText = m.style.cssText;
		var mi2 = document.createElement("div");
		mi2.className = mi.className;
		mi2.style.cssText = mi.style.cssText;
		m2.appendChild(mi2);
		twin.appendChild(m2);

		/* Swap node.elem for a dummy of the same size */
		dummy = document.createElement("div");
		dummy.style.width = node.elem.offsetWidth + "px";
		dummy.style.height = node.elem.offsetHeight + "px";

		mi2.appendChild(mi.replaceChild(dummy, node.elem));

		parent.appendChild(twin);

		updateattrdisplay();
	}

	var customonout = function(evt) {
		if (twin == null)
			return;

		parent.removeChild(twin);
		twin = null;

		mi.replaceChild(node.elem, dummy);
		dummy = null;

		updateattrdisplay();
	}

	updateattrdisplay();

	if (!node.selected || !node.leaf)
		return;

	mi.appendChild(document.createElement("br"));
	var q = document.createElement("span");
	q.className = "q";
	q.innerHTML = "?";
	mi.appendChild(q);
	attach(q, "mouseover", onover, false);
	attach(q, "mouseout", onout, false);
}

function helper_update_ruler_info(n) {
	var html;
	if (n.terminal)
		html = n.terminal.orth.to_xml_safe();
	else if (n.leaf) {
		html = "";

		var prev = 0;
		var p = (n.centre.length > 1) ? (1.0 / n.centre.length) :
			n.show_attribs ? 0.7 : 1.0;
		for (var i = 0; i < n.orth.length; i ++) {
			var sep = "";
			var orth = n.orth[i].to_xml_safe();
			if (orth[0] == " ") {
				if (i)
					sep = " ";
				orth = orth.substr(1);
			}

			var start = i + n.from;
			var count = 0;
			for (seg in n.centre)
				if (start >= n.centre[seg][0] &&
						start < n.centre[seg][1])
					count ++;

			if (count > prev)
				html += sep;
			if (count != prev) {
				if (prev)
					html += "</span>";
				if (count)
					html += "<span style=\"background: " +
						"rgba(255, 255, 255, " +
						(p * count) + ");\">";
			}
			if (count <= prev)
				html += sep;
			html += orth;

			prev = count;
		}
		if (prev)
			html += "</span>";
	} else
		html = "Błąd";
	if (n.terminal)
		html += "<br />" + n.terminal.base.to_xml_safe();
	if (n.attrs.tag)
		html += "<br />" + n.attrs.tag.to_xml_safe();

	n.ruler_span.innerHTML = html;
}

function helper_update_simple_ruler_info(n) {
	var html;
	if (n.terminal)
		html = n.terminal.orth.to_xml_safe();
	else if (n.leaf) {
		var orth = n.orth.join("");
		if (orth[0] == " ")
			orth = orth.substr(1);
		html = orth.to_xml_safe();
	} else
		html = "Błąd";

	if (n.terminal)
		html += "<br />" + n.terminal.base.to_xml_safe();

	if (n.attrs.tag)
		html += "<br />" + n.attrs.tag.to_xml_safe();

	n.ruler_span.innerHTML = html;
}

var s = [ "<span class=\"value\">", "</span>" ];
function helper_popup_info(node, popup, forest) {
	var link = document.createElement("a");
	link.innerHTML = "&lt;&lt; Szczegóły";
	link.className = "moreinfo";

	var para = document.createElement("p");
	para.appendChild(link);
	para.className = "moreinfo";
	popup.appendChild(document.createElement("br"));
	popup.appendChild(para);
	popup.className = "moreinfo";

	link.onclick = function(evt) {
		popup.removeChild(para);
		para.innerHTML = "";
		para.className = "";

		popup.appendChild(document.createElement("hr"));
		var section = document.createElement("h3");
		section.style.fontStyle = "italic";
		if (node.nonterminal)
			popup.appendChild(section);
		popup.appendChild(para);

		var html = "";
		html += node.terminal ?
				("Terminal " + s[0] +
				 node.terminal.orth + s[1]) :
				("Nieterminal " + s[0] +
				 node.nonterminal.category + s[1]);
		html += "<br />";

		html += "ID węzła: " + s[0] + node.nid + s[1];
		html += "<br />";

		if (node.nonterminal && !node.leaf) {
			html += "Wyprowadzony regułą " + s[0] +
				node.children[node.current].rule + s[1];
			html += "<br />";

			if (node.children.length > 5)
				html += "(możliwe " +
					(node.children.length - 1) +
					" innych sposobów wyprowadzenia)";
			else if (node.children.length > 2)
				html += "(możliwe " +
					(node.children.length - 1) +
					" inne sposoby wyprowadzenia)";
			else if (node.children.length > 1)
				html += "(możliwy 1 inny sposób wyprowadzenia)";
			else
				html += "(jednoznacznie)";
			html += "<br />";

			if (node.chtype == 1) {
				html += "Węzeł do rozstrzygnięcia";
				html += "<br />";
			} else {
				if (node.children.length > 1) {
					html += "Wybór " +
						((node.chtype == 4 ||
						  node.chtype == 5) ?
						 "dokonany przez użytkownika" :
						 "automatyczny");
					html += "<br />";
				}

				if (node.chtype == 2 || node.chtype == 3 ||
						node.chtype == 4) {
					html += "Wybór sugerowany";
					html += "<br />";
				}
			}

			html += "Wersja gramatyki: " + s[0] +
				forest.data.grammar_no + s[1];
			html += "<br />";
		} else if (node.nonterminal) {
			html += "Węzeł do rozstrzygnięcia";
		} else if (node.terminal) {
			html += "Forma podstatowa: " + s[0] +
				node.terminal.base + s[1];
			html += "<br />";

			html += "Interp_id: " + s[0] +
				node.terminal.interp_id + s[1];
			html += "<br />";
		}

		para.innerHTML = html;

		if (node.terminal)
			return;

		var set_section = function(sections) {
			for (var s in sections)
				for (var n in sections[s])
					if (node.nonterminal.category ==
							sections[s][n])
						section.innerHTML = s;
		}
		var set_rule = function(rules) {
			var rule = new RegExp("\\bs\\(" +
				node.children[node.current].rule + "\\)");
			var num = -1;
			for (var i in rules)
				if (rules[i].match(rule)) {
					num = i;
					break;
				}
			if (num == -1)
				return;
			para.innerHTML += "Treść reguły:<br /><pre>" +
				rules[num].to_xml_safe() + "</pre>";
		}
		gfjp_load("sections", "gfjp", set_section);
		gfjp_load("rules", "rules", set_rule);
	}
}

function helper_fill_node_attributes(node, display, add_attr_spans) {
	display.appendChild(document.createElement("hr"));

	var attrs = display.appendChild(document.createElement("table"));
	attrs.className = "attr-table";
	for (var n in node.attrs_order) {
		var name = node.attrs_order[n];
		var row = attrs.insertRow(-1);

		var attrdiv = document.createElement("div");
		var valdiv = document.createElement("div");

		add_attr_spans(attrdiv, name);
		add_attr_spans(valdiv, name, node.attrs[name]);

		row.insertCell(0).appendChild(attrdiv);
		row.insertCell(1).appendChild(valdiv);
		row.cells[0].className = "attr-name";

		if ("diffattrs" in node && name in node.diffattrs)
			row.className = "attr-diff";
	}
	display.appendChild(attrs);
}

var gfjp = {};
function gfjp_load(field, file, cb) {
	if (gfjp[field]) {
		cb(gfjp[field]);
		return;
	}

	gfjp.watch(field, function(prop, oldval, newval) {
				cb(newval);
				gfjp.unwatch(field);
				return newval;
			});

	var node = document.createElement("script");
	node.setAttribute("type", "text/javascript");
	node.setAttribute("src", "js/" + file + ".js");
	document.body.appendChild(node);
	if (!window.opera && window.navigator.userAgent.indexOf("WebKit") == -1)
		document.body.removeChild(node); /* Clean-up (?) */
}

function helper_after_loaded(forest) {
	if (!forest.blackboard || forest.rule)
		return;

	var rulename = "";
	var node = forest.startnode;
	if (node.children[0].rule)
		rulename = node.children[0].rule.to_xml_safe();

	forest.rule = document.createElement("span");
	forest.rule.className = "rule rule-choice";
	forest.rule.innerHTML = rulename;
	forest.blackboard.appendChild(forest.rule);
}

function helper_before_unload(forest) {
	if (!forest.blackboard || !forest.rule)
		return;
	forest.blackboard.removeChild(forest.rule);
	delete forest["rule"];
}

function helper_prepare_input(input) {
	if (!("forest" in input))
		return;
	for (var nid in input.forest.node) {
		var node = input.forest.node[nid];
		if (node.terminal) {
			node.hidden = true;
			continue;
		}
		if (node.leaf)
			node.incomplete = true;
		if (!node.nonterminal)
			continue;
		if ("children" in node && node.children.length > 1)
			for (var i in node.children)
				if ("chosen" in node.children[i])
					node.current = parseInt(i);
		if (!("f" in node.nonterminal)) {
			node.attrs = {};
			node.attrs_order = [];
			continue;
		}
		var f = node.nonterminal.f;
		delete node.nonterminal.f;
		if ("type" in f)
			f = [ f ];
		node.attrs = {};
		node.attrs_order = [];
		for (var i in f) {
			node.attrs_order.push(f[i].type);
			node.attrs[f[i].type] = f[i]["#text"];
		}
	}
}

var tree_helpers = {
	"prepare": helper_prepare_input,
	"update_node_info": helper_update_node_info,
	"update_ruler_info": helper_update_ruler_info,
	"popup_info": helper_popup_info,
};

var subtree_helpers = {
	"update_node_info": helper_update_node_info,
	"update_ruler_info": helper_update_ruler_info,
	"after_loaded": helper_after_loaded,
	"before_unload": helper_before_unload,
	"popup_info": helper_popup_info,
};

var filter_helpers = {
	"prepare": helper_prepare_filtered,
	"update_ruler_info": helper_update_simple_ruler_info,
};

var fulltree = null;
var helper_load = null;

function helper_loaded_full_subtree(num, viewer, button) {
	if (viewer.subtree_loaded)
		return;

	var subtree = {};
	for (var i in fulltree)
		subtree[i] = fulltree[i];

	subtree.stats = eval(uneval(subtree.stats));

	var node = helper_nodecopy(subtree.node[current_tree.disamb_node_id]);
	node.hidden = true;
	node.current = num;

	subtree.node[node.id] = node;
	subtree.stats.trees = node.subtrees;
	subtree.startnode = node;
	subtree.copy = true;

	viewer.orig_data = viewer.data;
	viewer.data = "";
	viewer.helper = tree_helpers;
	viewer.data = subtree;
	viewer.subtree_loaded = true;

	button.value = "Ukryj pełne poddrzewo";
	button.disabled = "";
}

function helper_load_full_subtree(url, num, obj, button) {
	if (obj.subtree_loaded) {
		obj.data = "";
		obj.subtree_loaded = false
		obj.helper = subtree_helpers;
		obj.data = obj.orig_data;
		delete obj.orig_data;

		button.value = "Pokaż pełne poddrzewo";
		button.disabled = "";
		return;
	}

	button.value = "Ładowanie…";
	button.disabled = "disabled";

	if (fulltree == null) {
		fulltree = -1;

		var node = document.createElement("script");
		node.setAttribute("type", "text/javascript");
		node.setAttribute("src", url);

		helper_load = function(result) {
			fulltree = result;
			helper_loaded_full_subtree(num, obj, button);

			document.body.removeChild(node); /* Clean-up (?) */
		}
		document.body.appendChild(node);
	}

	if (fulltree == -1)
		return;

	helper_loaded_full_subtree(num, obj, button);
}

function clean_up() {
	var emptyarray = new Array();
	for (var element in emptyarray)
		delete Array.prototype[element];
}
clean_up();
