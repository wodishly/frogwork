#version 410

precision highp float;

layout(location = 0) in vec4 position;
layout(location = 1) in vec2 uv;

out vec2 v_uv;
uniform mat4 u_modelview_matrix;
uniform mat4 u_projection_matrix;
uniform vec2 u_input2d;

void main() {
        v_uv = uv;

        vec4 p = position;
        vec2 i = u_input2d / 10000.0;

        float theta = mod(i.x, 2. * 3.1415);
        float c = cos(theta);
        float s = sin(theta);
        float px = c * p.x - s * p.z;
        float pz = s * p.x + c * p.z;

        vec4 coord = vec4(px , p.y, pz - 5.0+ i.y, 1.0);
        gl_Position = u_projection_matrix * u_modelview_matrix * coord;
}
