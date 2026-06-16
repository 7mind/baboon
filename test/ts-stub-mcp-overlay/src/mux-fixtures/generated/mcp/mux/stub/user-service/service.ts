import {In as createuser_In} from './createuser/in'
import {Out as createuser_Out} from './createuser/out'
import {In as getuser_In} from './getuser/in'
import {Out as getuser_Out} from './getuser/out'

export interface UserService {
    createUser(arg: createuser_In): createuser_Out;
    getUser(arg: getuser_In): getuser_Out;
}