#include "ISensorEasy.h"

ISensorEasy::ISensorEasy(Evolution::ResourceManager *rm) {
  resource_manager = rm;
  InitSensors();
}

ISensorEasy::~ISensorEasy(){
  ShutdownSensors();
}

void ISensorEasy::InitSensors(){ 
  // Get new resource container
  result = resource_manager->get_resource_container( 0, &resource_container );
  if( result != Evolution::RESULT_SUCCESS ) {
    throw "Cannot create resource container";
  }
  InitSensor(bn_ene,"IR_bn_ene");
  InitSensor(bn_wnw,"IR_bn_wnw");
  InitSensor(bn_n,"IR_bn_n");
  InitSensor(bn_ne,"IR_bn_ne");
  InitSensor(bn_nw,"IR_bn_nw");
  InitSensor(te_nnw,"IR_te_nnw");
  InitSensor(te_nne,"IR_te_nne");
  InitSensor(tw_nnw,"IR_tw_nnw");
  InitSensor(tw_nne,"IR_tw_nne");
  InitSensor(bs_w,"IR_bs_w");
  InitSensor(bs_e,"IR_bs_e");
  InitSensor(bw_s,"IR_bw_s");
  InitSensor(be_s,"IR_be_s");
}

void ISensorEasy::InitSensor(Evolution::IERRangeSensor * & sensor, char *interface_id){
  result = resource_container->obtain_interface(Evolution::NO_TICKET, interface_id,
						Evolution::IRangeSensor::INTERFACE_ID,
						(void**) &sensor);
  if (result != Evolution::RESULT_SUCCESS) {
    throw "Cannot create sensor interface";
  }
}

void ISensorEasy::InitBumpSensor(Evolution::IBumpSensor * & sensor, char *interface_id){
  result = resource_container->obtain_interface(Evolution::NO_TICKET, interface_id,
						Evolution::IBumpSensor::INTERFACE_ID,
						(void**) &sensor);
  if (result != Evolution::RESULT_SUCCESS) {
    throw "Cannot create sensor interface";
  }
}

void ISensorEasy::ShutdownSensors(){
  if( resource_manager != NULL ) delete resource_manager;
}
