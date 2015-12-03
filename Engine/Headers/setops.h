/*
 * Interface to April set handling functions
 */
#ifndef _SETOPS_H_
#define _SETOPS_H_
logical equalcell(register objPo c1,register objPo c2);

/* Escape interface */
retCode m_head(processpo p,objPo *args);
retCode m_front(processpo p,objPo *args);
retCode m_back(processpo p,objPo *args);
retCode m_tail(processpo p,objPo *args);
retCode m_listlen(processpo p,objPo *args);
retCode m_nth(processpo p,objPo *args);
retCode m_union(processpo p,objPo *args);
retCode m_sect(processpo p,objPo *args);
retCode m_diff(processpo p,objPo *args);
retCode m_app(processpo p,objPo *args);
retCode m_iota(processpo p,objPo *args);
retCode m_subset(processpo p,objPo *args);
retCode m_disjoint(processpo p,objPo *args);
retCode m_sort(processpo p,objPo *args);
#endif
