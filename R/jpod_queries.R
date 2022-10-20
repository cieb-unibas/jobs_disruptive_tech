JPOD_QUERIES <- list()

JPOD_QUERIES["total_nuts"] <- "
    SELECT COUNT(*) as total_postings, pc.nuts_2, rg.name_en AS Grossregion
    FROM position_characteristics pc
    LEFT JOIN (
        SELECT nuts_2, name_en
        FROM regio_grid 
        WHERE nuts_3 IS NULL AND nuts_2 IS NOT NULL
        ) rg on pc.nuts_2 = rg.nuts_2
    GROUP BY pc.nuts_2
    "

JPOD_QUERIES["bloom_nuts"] <- "
    SELECT COUNT(DISTINCT(pc.uniq_id)) as bloom_postings, pc.nuts_2, rg.name_en as Grossregion
    FROM position_characteristics pc 
    LEFT JOIN (
        SELECT nuts_2, name_en
        FROM regio_grid 
        WHERE nuts_3 IS NULL AND nuts_2 IS NOT NULL
        ) rg on pc.nuts_2 = rg.nuts_2
    WHERE pc.uniq_id IN (SELECT DISTINCT(bt.uniq_id) FROM bloom_tech bt)
    GROUP BY rg.name_en, pc.nuts_2
    "

JPOD_QUERIES["bloom_companies"] <- "
    SELECT pc.company_name, bt.bloom_field, COUNT(DISTINCT(jp.job_description)) as bloom_postings
    FROM (
        SELECT uniq_id, bloom_field
        FROM bloom_tech
        ) bt
    LEFT JOIN (
        SELECT uniq_id, company_name
        FROM position_characteristics
        ) pc on pc.uniq_id = bt.uniq_id
    LEFT JOIN (
        SELECT uniq_id, job_description
        FROM job_postings
        ) jp on jp.uniq_id = bt.uniq_id
    GROUP BY pc.company_name, bt.bloom_field
    ORDER BY bloom_postings DESC
    "