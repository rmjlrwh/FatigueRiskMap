-- ---------------------------------------------------------------------------------------
-- 1. Extract data for non fatigue presenters group, for further management in R
-- ---------------------------------------------------------------------------------------

-- -----------------------------------------------------------------------
	--  COHORT HAS BEEN REFINED AND SENT BACK FROM R
-- -----------------------------------------------------------------------


-- Reformat date columns
set sql_safe_updates=0;
alter table becky.fat3_nfp_cohort modify indexdate date;
alter table becky.fat3_nfp_cohort modify fu_start date;
alter table becky.fat3_nfp_cohort modify fu_end date;
alter table becky.fat3_nfp_cohort modify deathdate date;
alter table becky.fat3_nfp_cohort modify crd date;
alter table becky.fat3_nfp_cohort modify tod date;
alter table becky.fat3_nfp_cohort modify uts date;
alter table becky.fat3_nfp_cohort modify lcd date;
alter table becky.fat3_nfp_cohort modify yob_approx date;
alter table becky.fat3_nfp_cohort modify age30date date;
alter table becky.fat3_nfp_cohort modify age100date date;

CREATE INDEX `e_patid` ON becky.fat3_nfp_cohort (`e_patid`);
CREATE INDEX `indexdate` ON becky.fat3_nfp_cohort (`indexdate`);


-- -----------------------------------------------------------------------
	-- C) Extract diagnoses from CPRD for the provisional cohort
-- -----------------------------------------------------------------------
 
-- Extract diagnoses of interest for provisional fatigue cohort
-- Only for phenotyped diseases
-- within anytime before - 12 months after index date
-- No eligibility criteria or date range applied yet
drop table if exists  becky.fat3_nfp_diagnoses_events_cprd;
Create table becky.fat3_nfp_diagnoses_events_cprd 
ENGINE=MyISAM
select d.e_patid, d.eventdate, d.medcode, l.disease_number_new
from 18_299_Lyratzopoulos_e2.cprd_clinical d
inner join becky.fat3_nfp_cohort c on d.e_patid = c.e_patid
inner join becky.fat3_lookup_diseases_cprd l on d.medcode = l.medcode
where  d.eventdate <= date_add(c.indexdate, interval 12 month)

;

CREATE INDEX `e_patid` ON becky.fat3_nfp_diagnoses_events_cprd (`e_patid`);
CREATE INDEX `disease_number_new` ON becky.fat3_nfp_diagnoses_events_cprd (`disease_number_new`);
CREATE INDEX `medcode` ON becky.fat3_nfp_diagnoses_events_cprd (`medcode`);


-- -----------------------------------------------------------------------
	-- D) Extract diagnoses from HES ICD 10 codes for the provisional cohort
-- -----------------------------------------------------------------------

-- Extract diagnoses of interest for provisional fatigue cohort 
drop table if exists  becky.fat3_nfp_diagnoses_events_hes_icd;
Create table becky.fat3_nfp_diagnoses_events_hes_icd 
ENGINE=MyISAM
select d.e_patid, d.epistart, d.icd, l.disease_number_new
from 18_299_Lyratzopoulos_e2.hes_apc_diagnosis_epi d
inner join becky.fat3_nfp_cohort c on d.e_patid = c.e_patid
inner join becky.fat3_lookup_diseases_hes_icd10 l on d.icd = l.icd10code
where  d.epistart <= date_add(c.indexdate, interval 12 month)
;

CREATE INDEX `e_patid` ON becky.fat3_nfp_diagnoses_events_hes_icd (`e_patid`);
CREATE INDEX `disease_number_new` ON becky.fat3_nfp_diagnoses_events_hes_icd (`disease_number_new`);
CREATE INDEX `icd` ON becky.fat3_nfp_diagnoses_events_hes_icd (`icd`);


-- -----------------------------------------------------------------------
	-- E) Extract diagnoses from Cancer Registry ICD 10 codes for the provisional cohort
-- -----------------------------------------------------------------------

-- Extract diagnoses of interest for provisional fatigue cohort 
drop table if exists  becky.fat3_nfp_diagnoses_events_cr_icd;
Create table becky.fat3_nfp_diagnoses_events_cr_icd 
ENGINE=MyISAM
select d.e_patid, d.diagnosisdatebest, d.site_icd10_o2, l.disease_number_new
from 18_299_Lyratzopoulos_e2.cancer_registration_tumour d
inner join becky.fat3_nfp_cohort c on d.e_patid = c.e_patid
inner join becky.fat3_lookup_diseases_cr_icd10 l on d.site_icd10_o2 = l.icd10code
where  d.diagnosisdatebest <= date_add(c.indexdate, interval 12 month)
;

CREATE INDEX `e_patid` ON becky.fat3_nfp_diagnoses_events_cr_icd (`e_patid`);
CREATE INDEX `disease_number_new` ON becky.fat3_nfp_diagnoses_events_cr_icd (`disease_number_new`);
CREATE INDEX `site_icd10_o2` ON becky.fat3_nfp_diagnoses_events_cr_icd (`site_icd10_o2`);


-- -----------------------------------------------------------------------
	-- F) Extract diagnoses from HES OPCS for the provisional cohort
-- -----------------------------------------------------------------------


-- Extract diagnoses of interest for provisional fatigue cohort
drop table if exists  becky.fat3_nfp_diagnoses_events_hes_opcs;
Create table becky.fat3_nfp_diagnoses_events_hes_opcs 
ENGINE=MyISAM
select d.e_patid, d.epistart, d.opcs, l.disease_number_new
from 18_299_Lyratzopoulos_e2.hes_apc_procedures_epi d
inner join becky.fat3_nfp_cohort c on d.e_patid = c.e_patid
inner join becky.fat3_lookup_diseases_hes_opcs l on d.opcs = l.opcs4code
where  d.epistart <= date_add(c.indexdate, interval 12 month)
;

CREATE INDEX `e_patid` ON becky.fat3_nfp_diagnoses_events_hes_opcs (`e_patid`);
CREATE INDEX `disease_number_new` ON becky.fat3_nfp_diagnoses_events_hes_opcs (`disease_number_new`);
CREATE INDEX `icd` ON becky.fat3_nfp_diagnoses_events_hes_opcs (`opcs`);


-- -----------------------------------------------------------------------
	-- G) Combine diagnoses from CPRD and HES for fatigue AND ref cohorts
-- -----------------------------------------------------------------------


-- Apppend events from both sources
drop table if exists  becky.fat3_nfp_diagnoses_events_all;
create table becky.fat3_nfp_diagnoses_events_all

Select *
from

(
select e_patid, eventdate, disease_number_new, 
case when e_patid is not null then "cprd" else null end as data_source, 
medcode as code_number
from becky.fat3_nfp_diagnoses_events_cprd d

UNION ALL

select e_patid, epistart as eventdate, disease_number_new, 
case when e_patid is not null then "hes_icd" else null end as data_source,
icd as code_number
from becky.fat3_nfp_diagnoses_events_hes_icd d

UNION ALL

select e_patid, diagnosisdatebest as eventdate, disease_number_new, 
case when e_patid is not null then "cr_icd" else null end as data_source,
site_icd10_o2 as code_number
from becky.fat3_nfp_diagnoses_events_cr_icd d

UNION ALL

select e_patid, epistart as eventdate, disease_number_new, 
case when e_patid is not null then "hes_opcs" else null end as data_source,
opcs as code_number
from becky.fat3_nfp_diagnoses_events_hes_opcs d

) as tablename
;

CREATE INDEX `e_patid` ON becky.fat3_nfp_diagnoses_events_all (`e_patid`);
CREATE INDEX `eventdate` ON becky.fat3_nfp_diagnoses_events_all (`eventdate`);
CREATE INDEX `disease_number_new` ON becky.fat3_nfp_diagnoses_events_all (`disease_number_new`);

