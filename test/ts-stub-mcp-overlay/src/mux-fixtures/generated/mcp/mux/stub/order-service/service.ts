import {In as cancelorder_In} from './cancelorder/in'
import {Out as cancelorder_Out} from './cancelorder/out'
import {In as placeorder_In} from './placeorder/in'
import {Out as placeorder_Out} from './placeorder/out'

export interface OrderService {
    placeOrder(arg: placeorder_In): placeorder_Out;
    cancelOrder(arg: cancelorder_In): cancelorder_Out;
}