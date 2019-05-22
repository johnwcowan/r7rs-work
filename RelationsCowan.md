## Static methods

def dictToTuple(heading, d):  
def validateHeading(heading):  
def constraintFromCandidateKeyFactory(r, Hk=None, scope={}):  
def constraintFromForeignKeyFactory(r1, (r2, map), scope={}):  
def constraintFromLambdaFactory(r, f, scope={}):  
def _convertToShorthand(kn):  
def _convertToConstraint(kn):  
def relationFromCondition(f):  
def relationFromExtension(f):  
def AND(r1, r2):  
def OR(r1, r2):  
def MINUS(r1, r2):  
def REMOVE(r, Hr):  
def COMPOSE(r1, r2):  
def RESTRICT(r, restriction = lambda trx:True):  
def EXTEND(r, Hextension=[], extension = lambda trx:{}):  
def SEMIJOIN(r1, r2):  
def SEMIMINUS(r1, r2):  
def SUMMARIZE(r1, r2, exps):  
def GROUP(r, Hr, groupname):  
def UNGROUP(r, groupname):  
def WRAP(r, Hr, wrapname):  
def UNWRAP(r, wrapname):  
def DIVIDE_SIMPLE(r1, r2):  
def DIVIDE(r1, r2, r3, r4):  
def GENERATE(extension = {}):  
def TCLOSE(r):  
def QUOTA(r, limit, Hr=None, asc=True):  
def COUNT(r, none=None):  
def SUM(r, expression = lambda trx:None):  
def AVG(r, expression = lambda trx:None):  
def MAX(r, expression = lambda trx:None):  
def MIN(r, expression = lambda trx:None):  
def ALL(r, expression = lambda trx:None):  
def ANY(r, expression = lambda trx:None):  
def IS_EMPTY(r):

## Tuple methods

def __init__(self, _indict=None, args):  
def __getattr__(self, item):  
def __setattr__(self, item, value):  
def attributes(self):  
def __hash__(self):  
def __repr__(self):  
def remove(self, head):  
def project(self, head):  
def extend(self, Hextension = [], extension = lambda t:{}):  
def rename(self, newNames):  
def wrap(self, Hr, wrapname):  
def unwrap(self, wrapname):

## Relation methods:

def __init__(self, heading, body, constraints={'PK':(Key, None)}):  
def setConstraints(self, constraints):  
def __hash__(self):  
def __getstate__(self):  
def __setstate__(self,dict):  
def setBody(self, body):  
def __contains__(self, rel):  
def __eq__(self, rel):  
def __ne__(self, rel):  
def __lt__(self, rel):  
def __gt__(self, rel):  
def __le__(self, rel):  
def __ge__(self, rel):  
def __and__(self, rel):  
def __or__(self, rel):  
def __ior__(self, rel):  
def __sub__(self, rel):  
def __isub__(self, rel):  
def __copy__(self):  
def __len__(self):
