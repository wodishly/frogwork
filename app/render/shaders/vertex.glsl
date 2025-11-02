#version 410

precision highp float;

layout(location = 0) in vec4 position;
layout(location = 1) in vec2 uv;

out vec2 v_uv;
uniform vec2 u_input2d;

void main() {
        v_uv = uv;

        vec2 i = u_input2d / 10000.0;
        float theta = i.x;
        vec4 p = position;
        p.z *= -1.0;
        float c = cos(theta);
        float s = sin(theta);
        float px = c * p.x - s * p.z;
        float pz = s * p.x + c * p.z;

        gl_Position = vec4(px, p.y - i.y, pz, 1.0);
}
