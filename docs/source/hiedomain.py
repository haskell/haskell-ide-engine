from docutils import nodes
from sphinx import addnodes
from sphinx.directives import ObjectDescription
from sphinx.domains import Domain, ObjType
from sphinx.locale import l_, _
from sphinx.roles import XRefRole

class HIEDirective(ObjectDescription):
    def add_target_and_index(self, name, sig, signode):
        targetname = self.objtype + '-' + name
        if targetname not in self.state.document.ids:
            signode['names'].append(targetname)
            signode['ids'].append(targetname)
            signode['first'] = (not self.names)
            self.state.document.note_explicit_target(signode)

            objects = self.env.domaindata['rst']['objects']
            key = (self.objtype, name)
            if key in objects:
                self.env.warn(self.env.docname,
                              'duplicate description of %s %s, ' %
                              (self.objtype, name) +
                              'other instance in ' +
                              self.env.doc2path(objects[key]),
                              self.lineno)
            objects[key] = self.env.docname
        indextext = self.get_index_text(self.objtype, name)
        if indextext:
            self.indexnode['entries'].append(('single', indextext,
                                              targetname, ''))
    def get_index_text(self, objectname, name):
        if self.objtype == 'command':
            return _('%s (command)') % name
        if self.objtype == 'plugin':
            return _('%s (plugin)') % name
        return ''

class HIECommand(HIEDirective):
    def handle_signature(self, sig, signode):
        pluginname = self.env.ref_context.get('hie:plugin')
        signode += addnodes.desc_name('%s:%s' % (pluginname,sig), '%s' % sig)
        return sig

class HIEPlugin(HIEDirective):
    def handle_signature(self, sig, signode):
        signode += addnodes.desc_name('%s' % sig, '%s' % sig)
        return sig
    def before_content(self):
        self.env.ref_context['hie:plugin'] = self.names[0]
    def after_content(self):
        self.env.ref_context.pop('hie:plugin', None)

class HIEExample(HIEDirective):
    def handle_signature(self, sig, signode):
        signode += addnodes.desc_name('Example', 'Example')
        return sig

class HIEDomain(Domain):
    name = 'hie'
    label = 'haskell-ide-engine'
    object_types = {
        'command': ObjType(l_('command'), 'cmd'),
        'plugin': ObjType(l_('plugin'), 'plugin'),
        'example': ObjType(l_('example'), 'example')
    }
    directives = {
        'command': HIECommand,
        'plugin': HIEPlugin,
        'example': HIEExample
    }
    roles = {
        'cmd': XRefRole(),
        'plugin': XRefRole(),
        'example': XRefRole()
    }
    initial_data = {
        'objects': {},  # fullname -> docname, objtype
    }

    def clear_doc(self, docname):
        for (typ, name), doc in self.data['objects'].items():
            if doc == docname:
                del self.data['objects'][typ, name]

    def resolve_xref(self, env, fromdocname, builder, typ, target, node,
                     contnode):
        objects = self.data['objects']
        print(objects)
        objtypes = self.objtypes_for_role(typ)
        for objtype in objtypes:
            if (objtype, target) in objects:
                return make_refnode(builder, fromdocname,
                                    objects[objtype, target],
                                    objtype + '-' + target,
                                    contnode, target + ' ' + objtype)

    def get_objects(self):
        for (typ, name), docname in self.data['objects'].items():
            yield name, name, typ, docname, typ + '-' + name, 1

def setup(app):
    app.add_domain(HIEDomain)
    return {'version': '0.1'}
