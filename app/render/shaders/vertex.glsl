#version 410

precision highp float;

layout(location = 0) in vec4 position;
layout(location = 1) in vec2 uv;
layout(location = 2) in vec3 normal;
layout(location = 3) in ivec4 bones;
layout(location = 4) in vec4 weights;

uniform mat4 u_model_matrix;
uniform mat4 u_projection_matrix;
uniform mat4 u_view_matrix;

const int MAX_BONE_COUNT = 128;
uniform mat4 u_bone_matrices[MAX_BONE_COUNT];

out vec2 v_uv;
out vec3 v_normal;

void main() {
    v_uv = uv;

    vec4 transformedVertex = vec4(0.0);
    // todo transform normals
    if (weights.w == 1.0) {
        transformedVertex = position;
    } else {
        for (int i = 0; i < 4; i++) {
            vec4 transformed = u_bone_matrices[bones[i]] * position;
            transformedVertex += weights[i] * transformed;
        }
    }

    mat4 modelview_matrix = u_view_matrix * u_model_matrix;
    mat3 normal_matrix = transpose(inverse(mat3(modelview_matrix)));
    v_normal = normal_matrix * normal;

    gl_Position = u_projection_matrix * modelview_matrix * transformedVertex;
}
