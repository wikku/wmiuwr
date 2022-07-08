DROP TABLE IF EXISTS item;
DROP TABLE IF EXISTS category_parameter;
DROP TABLE IF EXISTS category;
DROP TABLE IF EXISTS parameter;

CREATE TABLE parameter (
	p_id int PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
	p_name text NOT NULL UNIQUE,
	p_type jsonb NOT NULL
	CHECK ((jsonb_typeof(p_type) = 'string' AND p_type::text IN ('"text"', '"numeric"'))
	    OR (jsonb_typeof(p_type) = 'array'))-- AND 'string' = ALL(jsonb_typeof(jsonb_array_elements(p_type)))))
);

-- TODO: change type representation or add trigger for checking if p_type is good

CREATE TABLE category (
	c_name text PRIMARY KEY
);

CREATE TABLE category_parameter (
	cp_category text REFERENCES category (c_name),
	cp_parameter int REFERENCES parameter (p_id),
	PRIMARY KEY (cp_category, cp_parameter)
);

CREATE TABLE item (
	i_id int PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
	i_category text REFERENCES category (c_name),
	i_attrs jsonb NOT NULL
);

-- TODO: add trigger for checking types of attrs and if all mandatory parameters are present
