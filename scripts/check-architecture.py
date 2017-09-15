#!/usr/bin/env python
from subprocess import Popen, PIPE
from pathlib import Path
import os
import sys
import yaml
import argparse
import re

parser = argparse.ArgumentParser(description='Check if architecture rules are being broken.')
parser.add_argument('configs', metavar='CONFIG_FILE', nargs='+',
                    help='YAML configuration file describing the architectural rules.')
parser.add_argument('-v', '--verbose', action='store_true', default=False)
args = parser.parse_args()

MODULEPART = r"[A-Z][a-zA-Z0-9_']*"
MODULENAME = r"{module_part}(\.{module_part})*".format(module_part = MODULEPART)

IMPORT_RE = re.compile(r'^import \s* (qualified)? \s* (?P<imported> {module_name})'
                           .format(module_name= MODULENAME), 
                       re.VERBOSE)
MODULE_NAME_RE = re.compile(r'^module \s* (?P<name> {module_name})'
                                .format(module_name = MODULENAME),
                            re.VERBOSE)


def flatten(pattern):
    if isinstance(pattern, str):
        yield pattern
    else:
        for item in pattern:
            yield from flatten(item)

def compile_module_patterns(patterns):
    return re.compile( 
       '^(' + '|'.join( compile_module_pattern(p) for p in flatten(patterns) ) + ')$'
    )

def compile_module_pattern(pattern):
    return ''.join( compile_module_pattern_part(part) for part in pattern.split('.') )[2:]
    
def compile_module_pattern_part(part):
    if part == '*':
        return r'\.{module_part}'.format(module_part=MODULEPART)
    elif part == '**':
        return r'(\.{module_part})*'.format(module_part=MODULEPART)
    else:
        return r'\.{module_part}'.format(module_part=re.escape(part))


class ForbidImport:

    def __init__(self, rule):
        self.level = rule['level']
        self.forbidden = compile_module_patterns(rule['forbidden'])
        self.within = compile_module_patterns(rule['within']) if 'within' in rule else None
        self.outside = compile_module_patterns(rule['outside']) if 'outside' in rule else None
        self.description = rule['description'].strip()

    def __str__(self):
        return '<ForbidImport level={} forbidden={} within={} outside={}>'.format(
                self.level, self.forbidden, self.within, self.outside)

    def check_module(self, module, verbose=False):
        if self.__should_check_module(module.name):
            if verbose: print('Checking module', module.name, '...', end=' ')

            no_errors = True
            for imported in module.imports:
                if self.forbidden.match(imported):
                    if verbose: print('✗', end=' ')
                    no_errors = False
                    yield {'level': self.level, 'description': 'Forbidden import of {} in {}'.format(imported, module.name)}
            if verbose:
              if no_errors: print('✓', end=' ')
              print()

    def __should_check_module(self, module):
        return ((self.within and self.within.match(module)) or
                (self.outside and not self.outside.match(module)))

class Module:

    def __init__(self, path):
        with open(path) as module:
            lines = module.readlines()
        import_matches = ( IMPORT_RE.match(line) for line in lines )
        self.imports = [ match.group('imported') for match in import_matches if match ]

        name_matches = ( MODULE_NAME_RE.match(line) for line in lines )
        self.name = next( match for match in name_matches if match ).group('name')

    def __str__(self):
        return '<Module name={} imports=[{}]>'.format(self.name, ','.join(self.imports))

def compile_rules(rules):
    for rule in rules:
        if rule['type'] == 'forbid-import':
            yield ForbidImport(rule)

def compile_modules(path):
    if path.is_dir():
        for entry in path.iterdir():
            yield from compile_modules(entry)
    elif path.suffix == '.hs':
        yield Module(path)

verbose = args.verbose
for config_file in args.configs:
    with open(config_file) as f:
        config = yaml.load(f)

    source_dir = Path(config['source-dir'])
    if not source_dir.exists():
        print('Error: source directory', source_dir, 'does not exist')
        sys.exit(1)
    if not source_dir.is_dir():
        print('Error: ', source_dir, 'is not a directory')
        sys.exit(1)
    modules = list(compile_modules(source_dir))


    any_errors = False
    for rule in compile_rules(config['rules']):
        if verbose: print('Checking rule: ', rule.description)
        problems = [ problem for module in modules for problem in rule.check_module(module, verbose=verbose) ]
        if verbose: print()

        if len(problems) > 0:
            print(rule.level + ': rule violation')
            print('  ', rule.description)
            print()
            for p in problems: 
                print('  ', p['description'])
            if rule.level == 'error':
                any_errors = True
            print()

    if any_errors: sys.exit(1)


