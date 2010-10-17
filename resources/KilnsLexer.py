from pygments.lexer import RegexLexer
from pygments.token import *

class DiffLexer(RegexLexer):
    name = 'Kilns'
    aliases = ['kilns']
    filenames = ['*.kiln']

    tokens = {
        'root': [
            (r';.*?$', Comment),
            (r'".*?"', String),
            (r'trigger|def|par|new|null', Keyword),
            (r'?\w+', Variable),
        ]
    }
