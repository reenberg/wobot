#ifndef __ISENSOREASY_H__
#define __ISENSOREASY_H__

#include <evolution/Resource.hpp>

class ISensorEasy{
 public:
  ISensorEasy(Evolution::ResourceManager *rm);
  ~ISensorEasy();
  Evolution::IERRangeSensor 
    *bn_ene, *bn_wnw, *bn_n,
    *bn_ne, *bn_nw,
    *te_nnw, *te_nne,
    *tw_nnw, *tw_nne,
	*bs_w, *bs_e, *bw_s, *be_s;
  Evolution::IBumpSensor *bump_w, *bump_e;
 private:
  void InitSensors();
  void InitSensor(Evolution::IERRangeSensor * & sensor, char *interface_id);
  void InitBumpSensor(Evolution::IBumpSensor * & sensor, char *interface_id);
  void ShutdownSensors();

  Evolution::Result result;
  Evolution::ResourceManager *resource_manager;
  Evolution::IResourceContainer *resource_container;
};
#endif
