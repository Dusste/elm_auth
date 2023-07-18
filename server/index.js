const loginRoute = require('./login-api');
const signupRoute = require('./signup-api');
const profileRoute = require('./profile-put-api');
const verifyRoute = require('./verify-put-api');
const socketRoute = require('./socket-api');
const messsagesRoute = require('./messages-api');
const forgotPasswordRoute = require('./forgot-password-api');
const resetPasswordRoute = require('./reset-password-api');

exports.routes = [
    loginRoute,
    signupRoute,
    profileRoute,
    verifyRoute,
    socketRoute,
    messsagesRoute,
    forgotPasswordRoute,
    resetPasswordRoute,
];
