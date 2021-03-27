DROP TABLE IF EXISTS term CASCADE;
DROP TABLE IF EXISTS cases CASCADE;
DROP TYPE IF EXISTS term_type;

CREATE TYPE term_type AS ENUM ('sort', 'var', 'lam', 'pi', 'app', 'fix', 'match', 'fixref', 'def', 'itype', 'icon', 'wildcard');

CREATE TABLE term (
	id BIGINT PRIMARY KEY,
	type_of BIGINT REFERENCES term(id),
	term_type term_type NOT NULL,
    size int4 NOT NULL,
	name VARCHAR CHECK (term_type in('sort', 'var', 'lam', 'pi', 'fix', 'match', 'def', 'itype', 'icon', 'wildcard') = (term_type IS NOT NULL)),
	t1 BIGINT REFERENCES term(id)
	 CHECK (term_type in ('lam', 'pi', 'app', 'fix', 'match') = (t1 IS NOT NULL)),
	t2 BIGINT REFERENCES term(id)
	 CHECK (term_type in ('lam', 'pi', 'app', 'match') = (t2 IS NOT NULL))
);

CREATE TABLE cases (
	id SERIAL PRIMARY KEY,
	parent BIGINT NOT NULL REFERENCES term(id),
	ord SERIAL NOT NULL,
	pattern BIGINT NOT NULL REFERENCES term(id),
	result	 BIGINT NOT NULL REFERENCES term(id),
	UNIQUE (parent, ord)
);