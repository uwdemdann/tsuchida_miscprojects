with dates as (
    select
        to_date('2022-04-01','yyyy-mm-dd') as start_date,
        to_date('2022-07-06','yyyy-mm-dd') as end_date
    from dual
)

select
    fee.pat_enc_csn_id
    ,disp.department_id
    ,dur.arrival as arrival_dttm
    ,to_number(to_char(dur.arrival,'hh24')) as arrival_hour
    --demo, etc. info
    ,fee.age_at_arrival_years
    ,cc.reason_visit_name
    ,esi.name as esi
    ,dd.day_of_week
    ,dd.weekend_yn
    --fields for analysis
    ----exposure
    ,case when fee.adt_arrival_date >= to_date('2022-07-11','yyyy-mm-dd') then 'Pilot' else 'Baseline' end as period --start date of pilot
    ,case when
        fee.first_emergency_department_id = 22122
            and disp.department_id = 22122
            and to_number(to_char(dur.arrival,'hh24')) between 11 and 15
            and dur.arrival_to_roomed > 15
            and dd.day_of_week in ('Monday','Tuesday','Wednesday','Thursday','Friday')
        then 1
        else 0 end as arriv_with_ambassador
    ----outcome(s)
    ,case when disp.final_flowsheet_disposition = 'Left Without Being Seen' then 1 else 0 end as left_lwbs
    ,case when disp.final_flowsheet_disposition = 'Left Before Exam Complete' then 1 else 0 end as left_lbec
    ,case when disp.final_flowsheet_disposition = 'Against Medical Advice' then 1 else 0 end as left_ama
    ,case when disp.final_flowsheet_disposition in ('Left Without Being Seen','Left Before Exam Complete','Against Medical Advice') then 1 else 0 end as left_lbtc
    ,dur.carestart_yn
    --all the times
    ,dur.arrival
    ,dur.roomed
    ,dur.departure
    ,dur.pat_in_transit_tac_to_uwed

from f_ed_encounters fee
cross join dates
inner join v_uwh_ed_disp_info disp on disp.pat_enc_csn_id = fee.pat_enc_csn_id
inner join v_uwh_ed_first_event_duration dur on dur.csn_dept = disp.csn_dept
inner join date_dimension dd on dd.calendar_dt = fee.adt_arrival_date

left outer join zc_acuity_level esi on esi.acuity_level_c = fee.acuity_level_c
left outer join cl_rsn_for_visit cc on cc.reason_visit_id = fee.first_chief_complaint_id




where
    fee.first_emergency_department_id in (22122,37001)
    and fee.first_emergency_department_id = fee.last_emergency_department_id
    and fee.adt_arrival_date between dates.start_date and dates.end_date
    and disp.visit_inclusion_grouper = 1
;