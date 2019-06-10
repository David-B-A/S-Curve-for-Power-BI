import { Visual } from "../../src/visual";
var powerbiKey = "powerbi";
var powerbi = window[powerbiKey];

var rscurveAB92463F20EE4FA295A2A717DF7E60A5_DEBUG = {
    name: 'rscurveAB92463F20EE4FA295A2A717DF7E60A5_DEBUG',
    displayName: 'rscurve',
    class: 'Visual',
    version: '1.0.0',
    apiVersion: '2.6.0',
    create: (options) => {
        if (Visual) {
            return new Visual(options);
        }

        console.error('Visual instance not found');
    },
    custom: true
};

if (typeof powerbi !== "undefined") {
    powerbi.visuals = powerbi.visuals || {};
    powerbi.visuals.plugins = powerbi.visuals.plugins || {};
    powerbi.visuals.plugins["rscurveAB92463F20EE4FA295A2A717DF7E60A5_DEBUG"] = rscurveAB92463F20EE4FA295A2A717DF7E60A5_DEBUG;
}

export default rscurveAB92463F20EE4FA295A2A717DF7E60A5_DEBUG;