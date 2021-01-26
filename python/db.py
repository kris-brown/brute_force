from psycopg2 import connect
from psycopg2.extensions import ISOLATION_LEVEL_AUTOCOMMIT
from psycopg2 import Error, ProgrammingError

from typing import FrozenSet as FS, Dict as D, Any  # List as L, Tuple as T
from dataclasses import dataclass, field
Conn = Any


def sqlexecute(conn: Conn, q: str, binds: list = []) -> list:  # type: ignore
    '''Run Postgres command, possibly with binds'''

    with conn.cursor() as cxn:
        cxn.execute(q, vars=binds)
        try:
            out = cxn.fetchall()
            return out  # type: ignore
        except ProgrammingError as e:
            if "no results to fetch" in e.args:
                return []
            else:
                raise Error(e)


def create_int(cxn: Conn, n: int = 100) -> None:
    '''Initialize the integer table'''

    cmds = ['''CREATE TABLE IF NOT EXISTS Int (
        id smallint NOT NULL,
        PRIMARY KEY (id))
    ''', 'INSERT INTO Int VALUES %s' % ','.join(
        ['(%d)' % i for i in range(n)])]

    for cmd in cmds:
        sqlexecute(cxn, cmd)


@dataclass(order=True, frozen=True)
class ConnectInfo(object):
    """
    PostGreSQL connection info
    """

    def connect(self, auto_commit: bool = True) -> Any:
        conn = connect(
            host="127.0.0.1",
            port=5432,
            user='ksb',
            password='ksb',
            dbname='brute',
            connect_timeout=28800,
        )
        conn.set_isolation_level(ISOLATION_LEVEL_AUTOCOMMIT)
        return conn


@dataclass(order=True, frozen=True)
class Schema:
    tabs: FS['Table']

    def make(self, cxn: ConnectInfo, nuke: bool) -> None:
        if nuke:
            safe_cxn = cxn.connect()
            sqlexecute(
                safe_cxn, 'DROP SCHEMA IF EXISTS brute CASCADE')
            sqlexecute(safe_cxn, 'CREATE SCHEMA brute')


@dataclass(order=True, frozen=True)
class Table:

    @property
    def name: str
    desc: str = ''
    fks: D[str, 'Table'] = field(default_factory=dict)

    def create(self, cxn: Conn) -> None:

        fks = ',\n\t'.join(['''CONSTRAINT {0}
                FOREIGN KEY({0})
                REFERENCES {1}(id)'''.format(*fk) for fk in self.fks.items()])
        create_str = '''CREATE TABLE IF NOT EXISTS {}(
            id SERIAL NOT NULL,
            PRIMARY KEY(id),{});'''.format(self.name, fks)

        tabdesc = "comment on table \"{}\" is '{}'".format(
            self.name, self.desc.replace("'", "''")
        )
        for sql in [create_str, tabdesc]:
            sqlexecute(cxn, sql)


f'''
create table {self.name}
    ( {self.name}_id serial primary key
    , {self.name}_type {self.name}_type not null
    , dog_name text
        check (({self.name}_type = 'dog') = (dog_name is not null))
    , dog_age integer
        check (({self.name}_type = 'dog') = (dog_age is not null))
    , bird_song text
        check (({self.name}_type = 'bird') = (bird_song is not null))
    );
'''
